use nom::multispace;
use std::str;

use common::{opt_multispace, statement_terminator, table_reference, value_list, Literal};
use table::Table;

#[derive(Clone, Debug, Default, Hash, PartialEq, Serialize, Deserialize)]
pub struct ExecuteStatement {
    pub table: Table,
    pub values: Vec<Literal>,
}

named!(pub execute_statement<&[u8], ExecuteStatement>,
    complete!(do_parse!(
        tag_no_case!("execute") >>
        multispace >>
        table: table_reference >>
        opt_multispace >>
        tag!("(") >>
        values: value_list >>
        tag!(")") >>
        statement_terminator >>
        ({
            // "table AS alias" isn't legal in INSERT statements
            assert!(table.alias.is_none());
            ExecuteStatement {
                table: table,
                values: values,
            }
        })
    ))
);
