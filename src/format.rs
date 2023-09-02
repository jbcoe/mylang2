use crate::{
    lexer::Lexer,
    token::{Kind, Token},
};

fn is_format(token: &Token) -> bool {
    matches!(token.kind(), Kind::Whitespace | Kind::EndOfFile)
}

pub fn format(source: &str) -> String {
    let tokens = Lexer::tokenize(source);
    let mut text = vec![];
    for token in tokens.iter().filter(|t| !is_format(t)) {
        match token.kind() {
            Kind::Semicolon => {
                // No space before a semicolon.
                if text.last() == Some(&' ') {
                    text.pop();
                }
                text.push(';');
                text.push('\n');
            }
            _ => {
                text.extend(token.text().chars());
                text.push(' ')
            }
        }
    }

    // Add trailing newline.
    match text.last() {
        Some(&'\n') => {}
        Some(_) => text.push('\n'),
        None => {}
    };
    text.iter().collect::<String>()
}

#[cfg(test)]
mod tests {
    use crate::format::format;

    #[test]
    fn test_format() {
        let source = "\
let  x =  1 ;let y = 2;
";
        let formatted_text = format(source);
        assert_eq!(
            formatted_text,
            "\
let x = 1;
let y = 2;
"
        );
    }

    #[test]
    fn test_no_op_format() {
        let source = "\
let x = 1;
let y = 2;
";
        let formatted_text = format(source);
        assert_eq!(
            formatted_text,
            "\
let x = 1;
let y = 2;
"
        );
    }
}
