use nom::character::complete::{multispace0, multispace1};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::str;

use column::Column;
use common::{
    assignment_expr_list, field_list, schema_table_reference, statement_terminator, value_list,
    ws_sep_comma, FieldValueExpression, Literal,
};
use keywords::escape_if_keyword;
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{map, opt};
use nom::multi::many1;
use nom::sequence::{delimited, preceded, tuple};
use nom::{IResult};
use select::{nested_selection, Selection};
use table::Table;

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct InsertStatement {
    pub table: Table,
    pub fields: Option<Vec<Column>>,
    pub data: InsertData,
    pub ignore: bool,
    pub on_duplicate: Option<Vec<(Column, FieldValueExpression)>>,
}

impl fmt::Display for InsertStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "INSERT INTO {}", escape_if_keyword(&self.table.name))?;
        if let Some(ref fields) = self.fields {
            write!(
                f,
                " ({})",
                fields
                    .iter()
                    .map(|ref col| col.name.to_owned())
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        write!(f, " VALUES {}", self.data,)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum InsertData {
    RowValueList(Vec<Vec<Literal>>),
    ValueList(Vec<Vec<Literal>>),
    Select(Selection),
}

impl Default for InsertData {
    fn default() -> Self {
        Self::ValueList(vec![vec![]])
    }
}

impl Display for InsertData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let fmt_values = |start, vvl: &Vec<Vec<Literal>>| {
            vvl.iter().fold(String::new(), |mut acc, vl| {
                if acc.len() > 0 {
                    acc.push_str(", ");
                }
                acc.push_str(&format!("{}(", start));
                acc.push_str(
                    &vl.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                );
                acc.push_str(")");
                acc
            })
        };

        match self {
            Self::ValueList(vl) => {
                write!(f, "{}", fmt_values("", vl))
            }
            Self::RowValueList(vl) => {
                write!(f, "{}", fmt_values("ROW", vl))
            }
            Self::Select(s) => {
                write!(f, "{}", s)
            }
        }
    }
}
fn fields(i: &[u8]) -> IResult<&[u8], Vec<Column>> {
    delimited(
        preceded(tag("("), multispace0),
        field_list,
        delimited(multispace0, tag(")"), multispace1),
    )(i)
}

fn data(i: &[u8]) -> IResult<&[u8], InsertData> {
    alt((
        // The ROW() method can't be mixed and matched so we match one or the other sets
        map(
            tuple((
                multispace1,
                many1(delimited(
                    tag("ROW("),
                    value_list,
                    preceded(tag(")"), opt(ws_sep_comma)),
                )),
            )),
            |(_, vl)| InsertData::RowValueList(vl),
        ),
        map(
            tuple((
                multispace0,
                many1(delimited(
                    tag("("),
                    value_list,
                    preceded(tag(")"), opt(ws_sep_comma)),
                )),
            )),
            |(_, vl)| InsertData::ValueList(vl),
        ),
    ))(i)
}

fn on_duplicate(i: &[u8]) -> IResult<&[u8], Vec<(Column, FieldValueExpression)>> {
    preceded(
        multispace0,
        preceded(
            tag_no_case("on duplicate key update"),
            preceded(multispace1, assignment_expr_list),
        ),
    )(i)
}

// Parse rule for a SQL insert query.
// TODO(malte): support REPLACE, nested selection, DEFAULT VALUES
pub fn insertion(i: &[u8]) -> IResult<&[u8], InsertStatement> {
    let (remaining_input, (_, ignore_res, _, _, _, table, _, fields, data, on_duplicate, _)) =
        tuple((
            tag_no_case("insert"),
            opt(preceded(multispace1, tag_no_case("ignore"))),
            multispace1,
            tag_no_case("into"),
            multispace1,
            schema_table_reference,
            multispace0,
            opt(fields),
            insertion_values,
            opt(on_duplicate),
            statement_terminator,
        ))(i)?;
    assert!(table.alias.is_none());
    let ignore = ignore_res.is_some();

    Ok((
        remaining_input,
        InsertStatement {
            table,
            fields,
            data,
            ignore,
            on_duplicate,
        },
    ))
}

pub fn insertion_values(i: &[u8]) -> IResult<&[u8], InsertData> {
    alt((
        map(nested_selection, |ns| InsertData::Select(ns)),
        map(tuple((tag_no_case("values"), data)), |(_, d)| d),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use arithmetic::{ArithmeticBase, ArithmeticExpression, ArithmeticOperator};
    use column::Column;
    use common::ItemPlaceholder;
    use table::Table;
    use FieldDefinitionExpression::Col;
    use {LiteralExpression, SelectStatement};

    #[test]
    fn simple_insert() {
        let qstring0 = "INSERT INTO users VALUES (42, \"test\");";
        let qstring1 = "INSERT INTO users VALUES ROW(42, \"test\");";

        let res0 = insertion(qstring0.as_bytes());
        let res1 = insertion(qstring1.as_bytes());
        assert_eq!(
            res0.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: None,
                data: InsertData::ValueList(vec![vec![42.into(), "test".into()]]),
                ..Default::default()
            }
        );
        assert_eq!(
            res1.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: None,
                data: InsertData::RowValueList(vec![vec![42.into(), "test".into()]]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn simple_insert_schema() {
        let qstring = "INSERT INTO db1.users VALUES (42, \"test\");";

        let res = insertion(qstring.as_bytes());
        assert_eq!(
            res.unwrap().1,
            InsertStatement {
                table: Table::from(("db1", "users")),
                fields: None,
                data: InsertData::ValueList(vec![vec![42.into(), "test".into()]]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn complex_insert() {
        let qstring = "INSERT INTO users VALUES (42, 'test', \"test\", CURRENT_TIMESTAMP);";

        let res = insertion(qstring.as_bytes());
        assert_eq!(
            res.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: None,
                data: InsertData::ValueList(vec![vec![
                    42.into(),
                    "test".into(),
                    "test".into(),
                    Literal::CurrentTimestamp,
                ],]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn insert_with_field_names() {
        let qstring = "INSERT INTO users (id, name) VALUES (42, \"test\");";

        let res = insertion(qstring.as_bytes());
        assert_eq!(
            res.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: Some(vec![Column::from("id"), Column::from("name")]),
                data: InsertData::ValueList(vec![vec![42.into(), "test".into()]]),
                ..Default::default()
            }
        );
    }

    // Issue #3
    #[test]
    fn insert_without_spaces() {
        let qstring0 = "INSERT INTO users(id, name) VALUES(42, \"test\");";
        let qstring1 = "INSERT INTO users(id, name) VALUESROW(42, \"test\");";

        let res0 = insertion(qstring0.as_bytes());
        let res1 = insertion(qstring1.as_bytes());
        assert_eq!(
            res0.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: Some(vec![Column::from("id"), Column::from("name")]),
                data: InsertData::ValueList(vec![vec![42.into(), "test".into()]]),
                ..Default::default()
            }
        );
        assert!(res1.is_err());
    }

    #[test]
    fn multi_insert() {
        let qstring0 = "INSERT INTO users (id, name) VALUES (42, \"test\"),(21, \"test2\");";
        let qstring1 = "INSERT INTO users (id, name) VALUES (42, \"test\"), (21, \"test2\");";
        let qstring2 = "INSERT INTO users (id, name) VALUES ROW(42, \"test\"),ROW(21, \"test2\");";
        let qstring3 = "INSERT INTO users (id, name) VALUES ROW(42, \"test\"), ROW(21, \"test2\");";
        let qstring4 = "INSERT INTO users (id, name) VALUES ROW(42, \"test\"),(21, \"test2\");";
        let qstring5 = "INSERT INTO users (id, name) VALUES (42, \"test\"),ROW(21, \"test2\");";

        let res0 = insertion(qstring0.as_bytes());
        let res1 = insertion(qstring1.as_bytes());
        let res2 = insertion(qstring2.as_bytes());
        let res3 = insertion(qstring3.as_bytes());
        let res4 = insertion(qstring4.as_bytes());
        let res5 = insertion(qstring5.as_bytes());

        let expected0 = InsertStatement {
            table: Table::from("users"),
            fields: Some(vec![Column::from("id"), Column::from("name")]),
            data: InsertData::ValueList(vec![
                vec![42.into(), "test".into()],
                vec![21.into(), "test2".into()],
            ]),
            ..Default::default()
        };
        let expected1 = expected0.clone();
        let expected2 = InsertStatement {
            table: Table::from("users"),
            fields: Some(vec![Column::from("id"), Column::from("name")]),
            data: InsertData::RowValueList(vec![
                vec![42.into(), "test".into()],
                vec![21.into(), "test2".into()],
            ]),
            ..Default::default()
        };
        let expected3 = expected2.clone();
        assert_eq!(res0.unwrap().1, expected0,);
        assert_eq!(res1.unwrap().1, expected1,);
        assert_eq!(res2.unwrap().1, expected2,);
        assert_eq!(res3.unwrap().1, expected3,);
        assert!(res4.is_err());
        assert!(res5.is_err());
    }

    #[test]
    fn insert_with_parameters() {
        let qstring = "INSERT INTO users (id, name) VALUES (?, ?);";

        let res = insertion(qstring.as_bytes());
        assert_eq!(
            res.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: Some(vec![Column::from("id"), Column::from("name")]),
                data: InsertData::ValueList(vec![vec![
                    Literal::Placeholder(ItemPlaceholder::QuestionMark),
                    Literal::Placeholder(ItemPlaceholder::QuestionMark)
                ]]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn insert_with_on_dup_update() {
        let qstring = "INSERT INTO keystores (`key`, `value`) VALUES ($1, :2) \
                       ON DUPLICATE KEY UPDATE `value` = `value` + 1";

        let res = insertion(qstring.as_bytes());
        let expected_ae = ArithmeticExpression::new(
            ArithmeticOperator::Add,
            ArithmeticBase::Column(Column::from("value")),
            ArithmeticBase::Scalar(1.into()),
            None,
        );
        assert_eq!(
            res.unwrap().1,
            InsertStatement {
                table: Table::from("keystores"),
                fields: Some(vec![Column::from("key"), Column::from("value")]),
                data: InsertData::ValueList(vec![vec![
                    Literal::Placeholder(ItemPlaceholder::DollarNumber(1)),
                    Literal::Placeholder(ItemPlaceholder::ColonNumber(2))
                ]]),
                on_duplicate: Some(vec![(
                    Column::from("value"),
                    FieldValueExpression::Arithmetic(expected_ae),
                ),]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn insert_with_leading_value_whitespace() {
        let qstring0 = "INSERT INTO users (id, name) VALUES ( 42, \"test\");";
        let qstring1 = "INSERT INTO users (id, name) VALUES ROW( 42, \"test\");";

        let res0 = insertion(qstring0.as_bytes());
        let res1 = insertion(qstring1.as_bytes());
        assert_eq!(
            res0.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: Some(vec![Column::from("id"), Column::from("name")]),
                data: InsertData::ValueList(vec![vec![42.into(), "test".into()]]),
                ..Default::default()
            }
        );
        assert_eq!(
            res1.unwrap().1,
            InsertStatement {
                table: Table::from("users"),
                fields: Some(vec![Column::from("id"), Column::from("name")]),
                data: InsertData::RowValueList(vec![vec![42.into(), "test".into()]]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn insert_select() {
        let qstring0 = "INSERT INTO users (id, name) SELECT id, name FROM dual;";
        let qstring1 = "INSERT INTO users (id, name) SELECT id, name FROM dual ON DUPLICATE KEY UPDATE name = 'dupe';";

        let res0 = insertion(qstring0.as_bytes());
        let res1 = insertion(qstring1.as_bytes());

        let expected0 = InsertStatement {
            table: Table::from("users"),
            fields: Some(vec![Column::from("id"), Column::from("name")]),
            data: InsertData::Select(Selection::Statement(SelectStatement {
                tables: vec![Table {
                    name: "dual".to_string(),
                    alias: None,
                    schema: None,
                }],
                distinct: false,
                fields: vec![
                    Col(Column {
                        name: "id".to_string(),
                        alias: None,
                        table: None,
                        function: None,
                    }),
                    Col(Column {
                        name: "name".to_string(),
                        alias: None,
                        table: None,
                        function: None,
                    }),
                ],
                join: vec![],
                where_clause: None,
                group_by: None,
                order: None,
                limit: None,
            })),
            ..Default::default()
        };

        let mut expected1 = expected0.clone();
        expected1.on_duplicate = Some(vec![(
            Column {
                name: "name".to_string(),
                alias: None,
                table: None,
                function: None,
            },
            FieldValueExpression::Literal(LiteralExpression {
                value: Literal::String("dupe".to_string()),
                alias: None,
            }),
        )]);

        assert_eq!(res0.unwrap().1, expected0);
        assert_eq!(res1.unwrap().1, expected1);
    }
}
