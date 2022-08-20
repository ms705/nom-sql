use std::fmt;
use std::fmt::{Display, Formatter};
use std::str;

use keywords::escape_if_keyword;

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Table {
    pub name: String,
    pub alias: Option<String>,
    pub schema: Option<String>,
}

impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref schema) = self.schema {
            write!(f, "{}.", escape_if_keyword(schema))?;
        }
        write!(f, "{}", escape_if_keyword(&self.name))?;
        if let Some(ref alias) = self.alias {
            write!(f, " AS {}", escape_if_keyword(alias))?;
        }
        Ok(())
    }
}

impl<'a> From<&'a str> for Table {
    fn from(t: &str) -> Table {
        Table {
            name: String::from(t),
            alias: None,
            schema: None,
        }
    }
}

impl<'a> From<(&'a str, &'a str)> for Table {
    fn from(t: (&str, &str)) -> Table {
        Table {
            name: String::from(t.1),
            alias: None,
            schema: Some(String::from(t.0)),
        }
    }
}

impl From<Table> for TableObject {
    fn from(table: Table) -> Self {
        Self {
            table,
            partitions: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct TableObject {
    pub table: Table,
    pub partitions: TablePartitionList,
}

impl Display for TableObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.table)?;

        if self.partitions.0.len() > 0 {
            write!(f, " {}", self.partitions)?;
        }

        Ok(())
    }
}

impl<'a> From<&'a str> for TableObject {
    fn from(t: &str) -> Self {
        TableObject {
            table: t.into(),
            partitions: Default::default(),
        }
    }
}
impl<'a> From<(&'a str, &'a str)> for TableObject {
    fn from(t: (&str, &str)) -> Self {
        TableObject {
            table: t.into(),
            partitions: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct TablePartitionList(pub Vec<TablePartition>);

impl Display for TablePartitionList {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PARTITION (")?;

        if self.0.len() > 0 {
            write!(
                f,
                "{}",
                self.0
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )?;
        }

        write!(f, ")")
    }
}

impl From<Vec<TablePartition>> for TablePartitionList {
    fn from(v: Vec<TablePartition>) -> Self {
        Self(v)
    }
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct TablePartition {
    name: String,
}

impl Display for TablePartition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> From<&'a str> for TablePartition {
    fn from(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl<'a> From<&'a [u8]> for TablePartition {
    fn from(name: &[u8]) -> Self {
        Self {
            name: String::from(str::from_utf8(name).unwrap()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use common::table_object;

    #[test]
    fn table_object_simple() {
        let qstring0 = "table1";
        let qstring1 = "schema1.table1";

        let res0 = table_object(qstring0.as_bytes());
        let res1 = table_object(qstring1.as_bytes());
        assert_eq!(res0.unwrap().1, TableObject::from("table1"),);
        assert_eq!(res1.unwrap().1, TableObject::from(("schema1", "table1")),);
    }

    #[test]
    fn table_object_with_alias() {
        let qstring0 = "table1 AS t1";
        let qstring1 = "schema1.table1 AS t1";

        let res0 = table_object(qstring0.as_bytes());
        let res1 = table_object(qstring1.as_bytes());
        assert_eq!(
            res0.unwrap().1,
            Table {
                name: "table1".to_string(),
                alias: Some("t1".to_string()),
                schema: None
            }
            .into()
        );
        assert_eq!(
            res1.unwrap().1,
            Table {
                name: "table1".to_string(),
                alias: Some("t1".to_string()),
                schema: Some("schema1".to_string()),
            }
            .into()
        );
    }

    #[test]
    fn table_object_with_partitioning() {
        let qstring0 = "schema1.table1 partition (a)";
        let qstring1 = "schema1.table1 partition (a, b1_long, a2b)";
        let qstring2 = "schema1.table1 AS t1 partition (a)";
        let qstring3 = "schema1.table1 AS t1 partition (a, b1_long, a2b)";

        let res0 = table_object(qstring0.as_bytes());
        let res1 = table_object(qstring1.as_bytes());
        let res2 = table_object(qstring2.as_bytes());
        let res3 = table_object(qstring3.as_bytes());
        assert_eq!(
            res0.unwrap().1,
            TableObject {
                table: Table {
                    name: "table1".to_string(),
                    alias: None,
                    schema: Some("schema1".to_string())
                },
                partitions: TablePartitionList(vec!["a".into(),])
            }
        );
        assert_eq!(
            res1.unwrap().1,
            TableObject {
                table: Table {
                    name: "table1".to_string(),
                    alias: None,
                    schema: Some("schema1".to_string())
                },
                partitions: TablePartitionList(vec!["a".into(), "b1_long".into(), "a2b".into(),])
            }
        );
        assert_eq!(
            res2.unwrap().1,
            TableObject {
                table: Table {
                    name: "table1".to_string(),
                    alias: Some("t1".to_string()),
                    schema: Some("schema1".to_string()),
                },
                partitions: TablePartitionList(vec!["a".into(),])
            }
        );
        assert_eq!(
            res3.unwrap().1,
            TableObject {
                table: Table {
                    name: "table1".to_string(),
                    alias: Some("t1".to_string()),
                    schema: Some("schema1".to_string()),
                },
                partitions: TablePartitionList(vec!["a".into(), "b1_long".into(), "a2b".into(),]),
            }
        );
    }
}
