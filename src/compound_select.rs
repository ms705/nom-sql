use nom::character::complete::{multispace0, multispace1};
use std::fmt;
use std::str;

use common::{opt_delimited, statement_terminator};
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::combinator::{map, opt};
use nom::multi::many1;
use nom::sequence::{preceded, tuple};
use nom::IResult;
use order::{order_clause, OrderClause};
use select::{limit_clause, nested_simple_selection, LimitClause, Selection};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub enum CompoundSelectOperator {
    Union,
    DistinctUnion,
    Intersect,
    Except,
}

impl fmt::Display for CompoundSelectOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CompoundSelectOperator::Union => write!(f, "UNION"),
            CompoundSelectOperator::DistinctUnion => write!(f, "UNION DISTINCT"),
            CompoundSelectOperator::Intersect => write!(f, "INTERSECT"),
            CompoundSelectOperator::Except => write!(f, "EXCEPT"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub struct CompoundSelectStatement {
    pub selects: Vec<(Option<CompoundSelectOperator>, Selection)>,
    pub order: Option<OrderClause>,
    pub limit: Option<LimitClause>,
}

impl fmt::Display for CompoundSelectStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (ref op, ref sel) in &self.selects {
            if op.is_some() {
                write!(f, " {}", op.as_ref().unwrap())?;
            }
            write!(f, " {}", sel)?;
        }
        if self.order.is_some() {
            write!(f, " {}", self.order.as_ref().unwrap())?;
        }
        if self.limit.is_some() {
            write!(f, " {}", self.order.as_ref().unwrap())?;
        }
        Ok(())
    }
}

// Parse compound operator
fn compound_op(i: &[u8]) -> IResult<&[u8], CompoundSelectOperator> {
    alt((
        map(
            preceded(
                tag_no_case("union"),
                opt(preceded(
                    multispace1,
                    alt((
                        map(tag_no_case("all"), |_| false),
                        map(tag_no_case("distinct"), |_| true),
                    )),
                )),
            ),
            |distinct| match distinct {
                // DISTINCT is the default in both MySQL and SQLite
                None => CompoundSelectOperator::DistinctUnion,
                Some(d) => {
                    if d {
                        CompoundSelectOperator::DistinctUnion
                    } else {
                        CompoundSelectOperator::Union
                    }
                }
            },
        ),
        map(tag_no_case("intersect"), |_| {
            CompoundSelectOperator::Intersect
        }),
        map(tag_no_case("except"), |_| CompoundSelectOperator::Except),
    ))(i)
}

// Parse terminated compound selection
pub fn compound_selection(i: &[u8]) -> IResult<&[u8], CompoundSelectStatement> {
    let (remaining_input, (compound_selection, _, _)) =
        tuple((nested_compound_selection, multispace0, statement_terminator))(i)?;

    Ok((remaining_input, compound_selection))
}

pub fn compound_selection_part(i: &[u8]) -> IResult<&[u8], Selection> {
    alt((
        map(compound_selection_compound_part, |cs| cs.into()),
        map(
            opt_delimited(tag("("), nested_simple_selection, tag(")")),
            |s| s.into(),
        ),
    ))(i)
}

pub fn compound_selection_compound_part(i: &[u8]) -> IResult<&[u8], CompoundSelectStatement> {
    let (remaining_input, (_, lhs, op_rhs, _)) = tuple((
        tag("("),
        opt_delimited(tag("("), nested_simple_selection, tag(")")),
        many1(tuple((multispace1, compound_op_selection_part))),
        tag(")"),
    ))(i)?;

    let mut css = CompoundSelectStatement {
        selects: vec![],
        order: None,
        limit: None,
    };

    css.selects.push((None, lhs.into()));

    for (_, (op, rhs)) in op_rhs {
        css.selects.push((Some(op), rhs.into()))
    }

    Ok((remaining_input, css))
}

pub fn compound_op_selection_part(i: &[u8]) -> IResult<&[u8], (CompoundSelectOperator, Selection)> {
    let (remaining_input, (op, _, selection)) =
        tuple((compound_op, multispace1, compound_selection_part))(i)?;

    Ok((remaining_input, (op, selection)))
}

// Parse nested compound selection
pub fn nested_compound_selection(i: &[u8]) -> IResult<&[u8], CompoundSelectStatement> {
    let (remaining_input, ((first, other_selects), order, limit)) = tuple((
        tuple((
            compound_selection_part,
            many1(tuple((multispace1, compound_op_selection_part))),
        )),
        opt(order_clause),
        opt(limit_clause),
    ))(i)?;

    let mut css = CompoundSelectStatement {
        selects: vec![],
        order,
        limit,
    };

    css.selects.push((None, first.into()));

    for os in other_selects {
        css.selects.push((Some(os.1 .0), os.1 .1.into()));
    }

    Ok((remaining_input, css))
}

#[cfg(test)]
mod tests {
    use super::*;
    use column::Column;
    use common::{FieldDefinitionExpression, FieldValueExpression, Literal};
    use select::selection;
    use table::Table;
    use SelectStatement;

    #[test]
    fn union() {
        let qstr = "SELECT id, 1 FROM Vote UNION SELECT id, stars from Rating;";
        let qstr2 = "(SELECT id, 1 FROM Vote) UNION (SELECT id, stars from Rating);";
        let res = selection(qstr.as_bytes());
        let res2 = selection(qstr2.as_bytes());

        let first_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            fields: vec![
                FieldDefinitionExpression::Col(Column::from("id")),
                FieldDefinitionExpression::Value(FieldValueExpression::Literal(
                    Literal::Integer(1).into(),
                )),
            ],
            ..Default::default()
        };
        let second_select = SelectStatement {
            tables: vec![Table::from("Rating")],
            fields: vec![
                FieldDefinitionExpression::Col(Column::from("id")),
                FieldDefinitionExpression::Col(Column::from("stars")),
            ],
            ..Default::default()
        };
        let expected = CompoundSelectStatement {
            selects: vec![
                (None, first_select.into()),
                (
                    Some(CompoundSelectOperator::DistinctUnion),
                    second_select.into(),
                ),
            ],
            order: None,
            limit: None,
        };

        assert_eq!(res.unwrap().1, expected.clone().into());
        assert_eq!(res2.unwrap().1, expected.into());
    }

    #[test]
    fn union_strict() {
        let qstr = "SELECT id, 1 FROM Vote);";
        let qstr2 = "(SELECT id, 1 FROM Vote;";
        let qstr3 = "SELECT id, 1 FROM Vote) UNION (SELECT id, stars from Rating;";
        let res = compound_selection(qstr.as_bytes());
        let res2 = compound_selection(qstr2.as_bytes());
        let res3 = compound_selection(qstr3.as_bytes());

        assert!(&res.is_err());
        assert_eq!(
            res.unwrap_err(),
            nom::Err::Error(nom::error::Error::new(
                ");".as_bytes(),
                nom::error::ErrorKind::MultiSpace
            ))
        );
        assert!(&res2.is_err());
        assert_eq!(
            res2.unwrap_err(),
            nom::Err::Error(nom::error::Error::new(
                ";".as_bytes(),
                nom::error::ErrorKind::Tag
            ))
        );
        assert!(&res3.is_err());
        assert_eq!(
            res3.unwrap_err(),
            nom::Err::Error(nom::error::Error::new(
                ") UNION (SELECT id, stars from Rating;".as_bytes(),
                nom::error::ErrorKind::MultiSpace
            ))
        );
    }

    #[test]
    fn multi_union() {
        let q = "SELECT id, 1 FROM Vote UNION SELECT id, stars from Rating UNION DISTINCT SELECT 42, 5 FROM Vote";
        let qstr0 = format!("{};", q);
        let qstr1 = format!("({}) UNION ALL ({});", q, q);
        let qstr2 = format!("{} UNION ALL {};", q, q);
        let res0 = selection(qstr0.as_bytes());
        let res1 = selection(qstr1.as_bytes());
        let res2 = selection(qstr2.as_bytes());

        let first_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            fields: vec![
                FieldDefinitionExpression::Col(Column::from("id")),
                FieldDefinitionExpression::Value(FieldValueExpression::Literal(
                    Literal::Integer(1).into(),
                )),
            ],
            ..Default::default()
        };
        let second_select = SelectStatement {
            tables: vec![Table::from("Rating")],
            fields: vec![
                FieldDefinitionExpression::Col(Column::from("id")),
                FieldDefinitionExpression::Col(Column::from("stars")),
            ],
            ..Default::default()
        };
        let third_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            fields: vec![
                FieldDefinitionExpression::Value(FieldValueExpression::Literal(
                    Literal::Integer(42).into(),
                )),
                FieldDefinitionExpression::Value(FieldValueExpression::Literal(
                    Literal::Integer(5).into(),
                )),
            ],
            ..Default::default()
        };

        let expected0 = CompoundSelectStatement {
            selects: vec![
                (None, first_select.clone().into()),
                (
                    Some(CompoundSelectOperator::DistinctUnion),
                    second_select.clone().into(),
                ),
                (
                    Some(CompoundSelectOperator::DistinctUnion),
                    third_select.clone().into(),
                ),
            ],
            order: None,
            limit: None,
        };

        let expected1 = CompoundSelectStatement {
            selects: vec![
                (None, expected0.clone().into()),
                (
                    Some(CompoundSelectOperator::Union),
                    expected0.clone().into(),
                ),
            ],
            order: None,
            limit: None,
        };

        let expected2 = CompoundSelectStatement {
            selects: vec![
                (None, first_select.clone().into()),
                (
                    Some(CompoundSelectOperator::DistinctUnion),
                    second_select.clone().into(),
                ),
                (
                    Some(CompoundSelectOperator::DistinctUnion),
                    third_select.clone().into(),
                ),
                (
                    Some(CompoundSelectOperator::Union),
                    first_select.clone().into(),
                ),
                (
                    Some(CompoundSelectOperator::DistinctUnion),
                    second_select.clone().into(),
                ),
                (
                    Some(CompoundSelectOperator::DistinctUnion),
                    third_select.into(),
                ),
            ],
            order: None,
            limit: None,
        };

        assert_eq!(res0.unwrap().1, expected0.into());
        assert_eq!(res1.unwrap().1, expected1.into());
        assert_eq!(res2.unwrap().1, expected2.into());
    }

    #[test]
    fn union_all() {
        let qstr = "SELECT id, 1 FROM Vote UNION ALL SELECT id, stars from Rating;";
        let res = selection(qstr.as_bytes());

        let first_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            fields: vec![
                FieldDefinitionExpression::Col(Column::from("id")),
                FieldDefinitionExpression::Value(FieldValueExpression::Literal(
                    Literal::Integer(1).into(),
                )),
            ],
            ..Default::default()
        };
        let second_select = SelectStatement {
            tables: vec![Table::from("Rating")],
            fields: vec![
                FieldDefinitionExpression::Col(Column::from("id")),
                FieldDefinitionExpression::Col(Column::from("stars")),
            ],
            ..Default::default()
        };
        let expected = CompoundSelectStatement {
            selects: vec![
                (None, first_select.into()),
                (Some(CompoundSelectOperator::Union), second_select.into()),
            ],
            order: None,
            limit: None,
        };

        assert_eq!(res.unwrap().1, expected.into());
    }
}
