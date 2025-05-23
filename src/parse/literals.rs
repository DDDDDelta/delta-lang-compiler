pub fn parse_string_literal(input: &str) -> Option<String> {
    assert!(input.len() >= 2, "String literal must be at least 2 characters long: \"\"");
    assert!(input.starts_with('"') && input.ends_with('"'), "String literal must start and end with double quotes");

    let mut result = String::new();
    let mut in_escape = false;

    for c in input.chars().skip(1) {
        if in_escape {
            match c {
                'n' => result.push('\n'),
                't' => result.push('\t'),
                '\\' => result.push('\\'),
                '"' => result.push('"'),
                _ => {
                    eprintln!("Unexpected escape sequence: \\{}", c);
                    return None;
                }
            }
            in_escape = false;
            continue;
        } 

        match c { 
            '\\' => in_escape = true,
            
            '\n' | '\r' => panic!("unexpected character in string literal, this should be catched in the lexer"),

            // End of string literal
            '"' => break, 

            _ => result.push(c),
        }
    }

    if in_escape {
        panic!("unexpected unclosed string literal, this should be catched in the lexer");
    }

    Some(result)
}
