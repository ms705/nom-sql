extern crate nom_sql;

use nom_sql::*;

fn main() {

    let sql = "DELETE FROM users WHERE user = $1 AND password = $2";
    let res = parse_query(sql);
    assert!(res.is_ok());

    let sql = "DELETE FROM users WHERE user = :1 AND password = :2";
    let res = parse_query(sql);
    assert!(res.is_ok());

    let sql = "DELETE FROM users WHERE user = ? AND password = ?";
    let res = parse_query(sql);
    assert!(res.is_ok());
}
