pub mod gnugen;
pub mod moira;
pub mod moira_inst;
pub mod moiragen;
//pub mod nasmgen;
// https://users.rust-lang.org/t/how-to-parameterize-a-function-or-macro-by-enum-variant/126398
#[macro_export]
macro_rules! expect {
    ($lexer:expr,$token:path) => {
        match $lexer.next_token() {
            Ok($token(data)) => data,
            Err(e) => return Err(e),
            t => bail!(
                "Expected {e:?}, got {t:?} {f} {l}",
                e = stringify!($token),
                f = file!(),
                l = line!()
            ),
        }
    };
}
