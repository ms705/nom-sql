use nom::character::complete::{multispace0, multispace1};
use std::fmt;
use std::str;

use column::SortingColumnIdentifier;
use common::{sorting_column_identifier, ws_sep_comma};
use condition::condition_expr;
use keywords::escape_if_keyword;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::combinator::{map, opt};
use nom::multi::many1;
use nom::sequence::{preceded, tuple};
use nom::IResult;
use ConditionExpression;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum OrderType {
    OrderAscending,
    OrderDescending,
}

impl fmt::Display for OrderType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OrderType::OrderAscending => write!(f, "ASC"),
            OrderType::OrderDescending => write!(f, "DESC"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum OrderingExpression {
    Columns(Vec<(SortingColumnIdentifier, OrderType)>),
    Condition(ConditionExpression),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct OrderClause {
    pub expression: OrderingExpression,
}

impl fmt::Display for OrderClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ORDER BY ")?;

        match &self.expression {
            OrderingExpression::Columns(c) => {
                write!(
                    f,
                    "{}",
                    c.iter()
                        .map(|&(ref c, ref o)| {
                            format!("{} {}", escape_if_keyword(&c.to_string()), o)
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            OrderingExpression::Condition(c) => {
                write!(f, "{}", c)
            }
        }
    }
}

pub fn order_type(i: &[u8]) -> IResult<&[u8], OrderType> {
    alt((
        map(tag_no_case("desc"), |_| OrderType::OrderDescending),
        map(tag_no_case("asc"), |_| OrderType::OrderAscending),
    ))(i)
}

fn order_expr(i: &[u8]) -> IResult<&[u8], OrderingExpression> {
    alt((
        map(many1(order_sorting_column), |c| {
            OrderingExpression::Columns(c)
        }),
        map(condition_expr, |c| OrderingExpression::Condition(c)),
    ))(i)
}

fn order_sorting_column(i: &[u8]) -> IResult<&[u8], (SortingColumnIdentifier, OrderType)> {
    let (remaining_input, (field_name, ordering, _)) = tuple((
        //column_identifier_no_alias,
        sorting_column_identifier,
        opt(preceded(multispace0, order_type)),
        opt(ws_sep_comma),
    ))(i)?;

    Ok((
        remaining_input,
        (field_name, ordering.unwrap_or(OrderType::OrderAscending)),
    ))
}

// Parse ORDER BY clause
pub fn order_clause(i: &[u8]) -> IResult<&[u8], OrderClause> {
    let (remaining_input, (_, _, _, oe)) = tuple((
        multispace0,
        tag_no_case("order by"),
        multispace1,
        order_expr,
    ))(i)?;

    Ok((remaining_input, OrderClause { expression: oe }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::Literal;
    use condition::ConditionBase::*;
    use condition::ConditionTree;
    use select::selection;
    use Column;
    use ConditionExpression::{Base, ComparisonOp};
    use {CaseWhenExpression, Operator};
    use {ColumnOrLiteral, FunctionArgument};

    #[test]
    fn order_by_clause() {
        let qstring1 = "select * from users order by name desc\n";
        let qstring2 = "select * from users order by name asc, age desc\n";
        let qstring3 = "select * from users order by name\n";

        let expected_ord1 = OrderClause {
            expression: OrderingExpression::Columns(vec![(
                SortingColumnIdentifier::FunctionArguments(FunctionArgument::Column("name".into())),
                OrderType::OrderDescending,
            )]),
        };
        let expected_ord2 = OrderClause {
            expression: OrderingExpression::Columns(vec![
                (
                    SortingColumnIdentifier::FunctionArguments(FunctionArgument::Column(
                        "name".into(),
                    )),
                    OrderType::OrderAscending,
                ),
                (
                    SortingColumnIdentifier::FunctionArguments(FunctionArgument::Column(
                        "age".into(),
                    )),
                    OrderType::OrderDescending,
                ),
            ]),
        };
        let expected_ord3 = OrderClause {
            expression: OrderingExpression::Columns(vec![(
                SortingColumnIdentifier::FunctionArguments(FunctionArgument::Column("name".into())),
                OrderType::OrderAscending,
            )]),
        };

        let res1 = selection(qstring1.as_bytes());
        let res2 = selection(qstring2.as_bytes());
        let res3 = selection(qstring3.as_bytes());
        assert_eq!(res1.unwrap().1.order, Some(expected_ord1));
        assert_eq!(res2.unwrap().1.order, Some(expected_ord2));
        assert_eq!(res3.unwrap().1.order, Some(expected_ord3));
    }

    #[test]
    fn order_by_case() {
        let qstring = "ORDER BY CASE WHEN vote_id > 10 THEN vote_id END DESC";

        let res = order_clause(qstring.as_bytes());

        let filter_cond = ComparisonOp(ConditionTree {
            left: Box::new(Base(Field(Column::from("vote_id")))),
            right: Box::new(Base(Literal(Literal::Integer(10.into())))),
            operator: Operator::Greater,
        });
        let expected = OrderClause {
            expression: OrderingExpression::Columns(vec![(
                SortingColumnIdentifier::FunctionArguments(FunctionArgument::Conditional(
                    CaseWhenExpression {
                        then_expr: ColumnOrLiteral::Column(Column::from("vote_id")),
                        else_expr: None,
                        condition: filter_cond,
                    },
                )),
                OrderType::OrderDescending,
            )]),
        };

        assert_eq!(res.unwrap().1, expected);
    }

    #[test]
    fn order_by_positionals() {
        let qstring0 = "ORDER BY 1";
        let qstring1 = "ORDER BY 1, 5, 3";

        let res0 = order_clause(qstring0.as_bytes());
        let res1 = order_clause(qstring1.as_bytes());

        let expected0 = OrderClause {
            expression: OrderingExpression::Columns(vec![(
                SortingColumnIdentifier::Position(1),
                OrderType::OrderAscending,
            )]),
        };
        let expected1 = OrderClause {
            expression: OrderingExpression::Columns(vec![
                (
                    SortingColumnIdentifier::Position(1),
                    OrderType::OrderAscending,
                ),
                (
                    SortingColumnIdentifier::Position(5),
                    OrderType::OrderAscending,
                ),
                (
                    SortingColumnIdentifier::Position(3),
                    OrderType::OrderAscending,
                ),
            ]),
        };

        assert_eq!(res0.unwrap().1, expected0);
        assert_eq!(res1.unwrap().1, expected1);
    }
}
