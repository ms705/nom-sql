use common::is_sql_identifier;
use keywords::sql_keyword;
use nom::character::complete::{multispace0, multispace1};
use select::{nested_selection, SelectStatement};
use std::{fmt, str};

use nom::bytes::complete::{tag, tag_no_case, take_while1};
use nom::combinator::{map, not, peek};
use nom::multi::separated_list;
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct WithDefinition {
    pub name: String,
    pub select_statement: Box<SelectStatement>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct WithClause {
    pub with_definitions: Vec<WithDefinition>,
}

impl fmt::Display for WithClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "WITH ")?;
        for def in self.with_definitions.iter() {
            write!(f, "{} AS ( {} )", def.name, def.select_statement)?;
        }
        Ok(())
    }
}

pub fn with_clause(i: &[u8]) -> IResult<&[u8], WithClause> {
    let (remaining_input, (_, _, _, with_definitions, _)) = tuple((
        multispace0,
        tag_no_case("with"),
        multispace1,
        separated_list(
            tag(","),
            map(
                tuple((
                    with_identifier,
                    delimited(multispace1, tag_no_case("as"), multispace0),
                    map(
                        delimited(
                            tag("("),
                            delimited(multispace0, nested_selection, multispace0),
                            tag(")"),
                        ),
                        |s| Box::new(s),
                    ),
                )),
                |(name, _, select_statement)| WithDefinition {
                    name: String::from(str::from_utf8(name).unwrap()),
                    select_statement,
                },
            ),
        ),
        multispace0,
    ))(i)?;
    Ok((remaining_input, WithClause { with_definitions }))
}

fn with_identifier(i: &[u8]) -> IResult<&[u8], &[u8]> {
    preceded(not(peek(sql_keyword)), take_while1(is_sql_identifier))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use select::selection;

    #[test]
    fn simple_with() {
        let qstring = "with sample_name as ( select * from users )";
        let res = with_clause(qstring.as_bytes());
        println!("{:?}", res);
    }

    #[test]
    fn multiple_with() {
        let qstring =
            "with sample1 as ( select * from users ), sample2 as ( select * from address )";
        let res = with_clause(qstring.as_bytes());
        println!("{:?}", res);
    }

    #[test]
    fn with_selection() {
        let qstring = "with sample_name as ( select * from users ) select * from sample_name\n";
        let res = selection(qstring.as_bytes());
        println!("{:?}", res);
    }
}
