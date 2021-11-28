use core::panic;
use std::{
    fmt::Display,
    fs::{read_to_string, File},
    io::Write,
    iter::Peekable,
    path::Path,
    str::{Chars, FromStr},
};
use thiserror::Error;

pub struct Tablatal<R> {
    headers: Vec<(String, usize)>,
    records: Vec<R>,
}

impl<R: FromRecord> Tablatal<R> {
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, ParseError> {
        read_to_string(path)?.parse()
    }

    pub fn from_records(records: Vec<R>) -> Self {
        Self {
            headers: vec![],
            records,
        }
    }
}

impl<R: FromRecord> FromStr for Tablatal<R> {
    type Err = ParseError;

    fn from_str(content: &str) -> Result<Self, Self::Err> {
        let mut lines = content.lines().enumerate();

        let mut tbtl = Self {
            headers: vec![],
            records: vec![],
        };

        tbtl.headers = match lines.next() {
            Some((_, s)) => Self::read_headers(s)?,
            None => return Ok(tbtl),
        };

        for (line_number, raw_record) in lines {
            let mut raw_chars = raw_record.chars();
            let mut raw_fields = vec![];

            // Get all fixed length fields
            for (header_name, record_length) in tbtl.headers.iter().rev().skip(1).rev() {
                let field: String = (&mut raw_chars).take(*record_length).collect();

                if field.len() != *record_length {
                    return Err(ParseError::IncompleteRecord {
                        line_number,
                        header_name: header_name.clone(),
                    });
                }

                raw_fields.push(field);

                match raw_chars.next() {
                    Some(ch) if ch != ' ' => panic!("No space between fields"),
                    _ => (),
                }
            }

            // The last field is just the rest of the line
            raw_fields.push(raw_chars.collect());

            let borrowed: Vec<&str> = raw_fields.iter().map(|owned| owned.trim()).collect();
            let record = R::from_record(&borrowed).map_err(|err| ParseError::FailedFromRecord {
                line_number,
                error: format!("{}", err),
            })?;

            tbtl.records.push(record);
        }

        Ok(tbtl)
    }
}

impl<R> Tablatal<R> {
    fn read_headers<S: AsRef<str>>(head: S) -> Result<Vec<(String, usize)>, ParseError> {
        let read_header = |chars: &mut Peekable<Chars>| -> (String, usize) {
            let header_name: String = Self::peek_take_string(chars, |c| !c.is_whitespace());
            let mut whitespace_length = Self::peek_take_string(chars, |c| c.is_whitespace()).len();

            // Remove one whitesace length for the mandatory space between fields
            if whitespace_length > 0 {
                whitespace_length -= 1
            }

            let length = header_name.len() + whitespace_length;

            (header_name, length)
        };

        let mut chars = head.as_ref().chars().peekable();

        // Be sure the headers start with a name
        match chars.peek() {
            Some(c) if c.is_whitespace() => return Err(ParseError::HeadersStartWhitespace),
            _ => (),
        }

        let mut headers = vec![];
        loop {
            headers.push(read_header(&mut chars));

            match chars.peek() {
                None => return Ok(headers),
                _ => (),
            }
        }
    }

    fn peek_take_string<P>(chars: &mut Peekable<Chars>, mut predicate: P) -> String
    where
        P: FnMut(&char) -> bool,
    {
        let mut taken = String::new();

        while let Some(ch) = chars.next_if(&mut predicate) {
            taken.push(ch)
        }

        return taken;
    }
}

impl<R: AsRecord> Tablatal<R> {
    pub fn new() -> Self {
        Self {
            headers: vec![],
            records: vec![],
        }
    }

    pub fn push(&mut self, record: R) {
        self.records.push(record)
    }

    pub fn as_string(&self) -> Result<String, WriteError> {
        // TODO: Use a read-file's headers instead of the ones on AsRecord
        let headers = R::headers();

        let mut field_lengths = vec![0; headers.len()];
        for (index, header) in headers.iter().enumerate() {
            field_lengths[index] = header.len();
        }

        let mut records: Vec<Vec<String>> = vec![];
        for data in &self.records {
            let record = data.as_record();

            if record.len() != headers.len() {
                return Err(WriteError::HeaderRecordLength {
                    records_length: record.len(),
                    headers_length: headers.len(),
                });
            }

            for (index, field) in record.iter().enumerate() {
                field_lengths[index] = field_lengths[index].max(field.len());
            }

            records.push(record);
        }

        let pad = |mut field: String, total_length: usize| -> String {
            let pad: String = [' '].repeat(total_length - field.len()).iter().collect();

            field.push_str(&pad);
            field
        };

        let mut ret = String::new();

        for (index, header) in headers.iter().enumerate().take(headers.len() - 1) {
            ret.push_str(&pad(header.to_string(), field_lengths[index]));
            ret.push(' ');
        }
        ret.push_str(headers.last().unwrap());
        ret.push('\n');

        for record in records {
            for (index, record) in record.iter().enumerate().take(record.len() - 1) {
                ret.push_str(&pad(record.to_string(), field_lengths[index]));
                ret.push(' ');
            }
            ret.push_str(record.last().unwrap());
            ret.push('\n');
        }

        Ok(ret)
    }

    pub fn save_file<P: AsRef<Path>>(&self, path: P) -> Result<(), WriteError> {
        let string = self.as_string()?;

        let mut file = File::create(path)?;
        //TODO: Use a different error here than `WriteError::IoError` (and rename that one)
        // so we don't pritn "Unable to open file" for this error, too
        file.write_all(string.as_bytes()).map_err(|e| e.into())
    }
}

impl<R> Tablatal<R> {
    pub fn records(&self) -> &[R] {
        &self.records
    }

    pub fn records_mut(&mut self) -> &mut [R] {
        &mut self.records
    }

    pub fn into_records(self) -> Vec<R> {
        self.records
    }
}

pub trait AsRecord {
    /// Stringify into records. The returned Vec should be the same length as
    /// [AsRecord::headers] and the slice taken by [FromRecord::from_record]
    fn as_record(&self) -> Vec<String>;

    /// The column titles. The returned Vec should be the same length as
    /// [AsRecord::as_records] and the slice taken by [FromRecord::from_record]
    fn headers() -> &'static [&'static str];
}

pub trait FromRecord: Sized {
    type Err: Display;

    fn from_record(rcd: &[&str]) -> Result<Self, Self::Err>;
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Could not read the file {0}")]
    IoError(#[from] std::io::Error),
    #[error("The first record may not start with whitespace")]
    HeadersStartWhitespace,
    #[error(
        "A record ended early! Line {line_number} did not have any data for the {header_name} header"
    )]
    IncompleteRecord {
        line_number: usize,
        header_name: String,
    },
    #[error("Record failed to parse on line {line_number}: {error}")]
    FailedFromRecord { line_number: usize, error: String },
}

#[derive(Debug, Error)]
pub enum WriteError {
    #[error("Unable to open file for writing {0}")]
    IoError(#[from] std::io::Error),
    #[error(
        "The length of the headers and the records need to match. Records were {records_length} and Headers {headers_length}!"
    )]
    HeaderRecordLength {
        records_length: usize,
        headers_length: usize,
    },
}

#[cfg(test)]
mod test {
    use std::{fs::File, io::Write};

    use super::*;

    #[derive(Debug, PartialEq)]
    struct Record {
        id: usize,
        hash: Vec<char>,
        name: String,
    }

    impl FromRecord for Record {
        type Err = String;

        fn from_record(rcd: &[&str]) -> Result<Self, Self::Err> {
            let id: usize = rcd[0].trim().parse().unwrap();
            let hash = rcd[1].chars().collect();
            let name = rcd[2].to_owned();

            Ok(Self { id, hash, name })
        }
    }

    impl AsRecord for Record {
        fn as_record(&self) -> Vec<String> {
            let mut record = vec![];
            record.push(format!("{}", self.id));
            record.push(self.hash.iter().collect());
            record.push(self.name.clone());

            record
        }

        fn headers() -> &'static [&'static str] {
            &["ID", "HASH", "NAME"]
        }
    }

    #[test]
    fn parse_headers() {
        let header_line = String::from("ID   HASH    NAME");

        assert_eq!(
            Tablatal::<()>::read_headers(header_line).unwrap(),
            vec![
                (String::from("ID"), 4),
                (String::from("HASH"), 7),
                (String::from("NAME"), 4)
            ]
        )
    }

    #[test]
    fn parse_one() {
        let raw = String::from("ID   HASH    NAME\n0    abcdefg name1");
        let tbtl: Tablatal<Record> = raw.parse().unwrap();

        assert_eq!(
            tbtl.headers,
            vec![
                (String::from("ID"), 4),
                (String::from("HASH"), 7),
                (String::from("NAME"), 4)
            ]
        );

        assert_eq!(
            tbtl.records,
            vec![Record {
                id: 0,
                hash: "abcdefg".chars().collect(),
                name: String::from("name1")
            }]
        )
    }

    #[test]
    fn parse() {
        let raw = String::from("ID   HASH    NAME\n0    abcdefg name1\n1    hijklmn name2");
        let tbtl: Tablatal<Record> = raw.parse().unwrap();

        assert_eq!(
            tbtl.headers,
            vec![
                (String::from("ID"), 4),
                (String::from("HASH"), 7),
                (String::from("NAME"), 4)
            ]
        );

        assert_eq!(
            tbtl.records,
            vec![
                Record {
                    id: 0,
                    hash: "abcdefg".chars().collect(),
                    name: String::from("name1")
                },
                Record {
                    id: 1,
                    hash: "hijklmn".chars().collect(),
                    name: String::from("name2")
                }
            ]
        )
    }

    #[test]
    fn write_headers() {
        let mut tbtl = Tablatal::new();
        tbtl.push(Record {
            id: 3,
            hash: "abcdefg".chars().collect(),
            name: String::from("name1"),
        });

        assert_eq!(
            tbtl.as_string().unwrap(),
            String::from("ID HASH    NAME\n3  abcdefg name1\n")
        );

        File::create("example.tbtl")
            .unwrap()
            .write_all(tbtl.as_string().unwrap().as_bytes())
            .unwrap()
    }
}
