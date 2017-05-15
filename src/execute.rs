use nom::multispace;
use nom::{IResult, Err, ErrorKind, Needed};
use std::str;

use common::{statement_terminator, table_reference, value_list, Literal};
use table::Table;

#[derive(Clone, Debug, Default, Hash, PartialEq, Serialize, Deserialize)]
pub struct ExecuteStatement {
    pub table: Table,
    pub values: Vec<Literal>,
}

named!(pub execute_statement<&[u8], ExecuteStatement>,
    complete!(chain!(
        caseless_tag!("execute") ~
        multispace ~
        table: table_reference ~
        opt!(multispace) ~
        tag!("(") ~
        values: value_list ~
        tag!(")") ~
        statement_terminator,
        || {
            // "table AS alias" isn't legal in INSERT statements
            assert!(table.alias.is_none());
            ExecuteStatement {
                table: table,
                values: values,
            }
        }
    ))
);
