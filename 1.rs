
impl<R: std::io::Read> Lexical<R> {

    pub fn tokenize(mut self) -> Vec<Token> {
        while let c = self.r.next() {
            if c == '\0' {
                // fetch the last frame as much as possible
            if self.s.top() == Some(&State::Undetermined) && !self.t.is_empty() {
                    self.z.push(self.infer_from_str_like(self.t_string().as_str()))
                }
                break;
            }
            if let Some(s) = self.s.top() {
                match s {
                    State::Value => self.exp_value(c),
                    State::Comment => self.exp_comment(c),
                    State::Dictionary => self.exp_dict(c),
                    // State::DictPair => self.exp_dict_pair(c),
                    State::Tuple => self.exp_tuple(c),
                    State::List => self.exp_list(c),
                    State::Pattern => self.exp_pattern(c),
                    State::Undetermined => self.exp_undetermined(c),
                    State::StrWrappedInSingleQuotationMarks => self.exp_string1(c),
                    State::StrWrappedInDoubleQuotationMarks => self.exp_string2(c),
                    State::FnName => self.exp_fn_name(c),
                    State::FnCallChain => self.exp_fn_call_chain(c),
                    State::Variable => self.exp_variable(c),
                    State::UninterruptibleChars => self.exp_uninterruptible_chars(c),
                    State::Error => self.error(c),
                    State::TupleElement => {}
                    State::ListElement => {}
                    State::VariableStart => {}
                    State::BoxValue => self.exp_box_value(c),
                    State::BoxPattern => self.exp_box_pattern(c),
                };
            }
        }
        self.z
    }

    //// State::BoxPattern
    fn exp_box_pattern(&mut self, c: char) {
        match c {
            // stop
            '=' if self.r.peek() == '>' => {
                self.s.pop_and_push(State::BoxValue);
                self.r.skip(1);
                self.z.push(Token::Arrow);
            }
            // comments
            '/' if self.r.peek_more(2) == "**" => {
                self.r.skip(2);
                self.s.push(State::Comment);
                self.z.push(Token::CommentStart);
            }
            // ignore
            '\t' | '\r' | '\n' | '\x20' => {}
            // into
            // how to avoid Repetition? that's for the parser
            _ => {
                self.s.push(State::Pattern);
                self.exp_pattern(c);
            }
        }
    }

    //// State::BoxValue
    fn exp_box_value(&mut self, c: char) {
        match c {
            // stop
            ',' | ')' | ']' | '}' => {
                self.s.pop();
                match self.s.top() {
                    Some(State::Dictionary) => self.exp_dict(c),
                    Some(State::Tuple) => self.exp_tuple(c),
                    Some(State::List) => self.exp_list(c),
                    _ => self.error(c),
                }
            }
            // comments
            '/' if self.r.peek_more(2) == "**" => {
                self.r.skip(2);
                self.s.push(State::Comment);
                self.z.push(Token::CommentStart);
            }
            // ignore
            '\t' | '\r' | '\n' | '\x20' => {}
            // into
            // how to avoid Repetition? that's for the parser
            _ => {
                self.s.push(State::Value);
                self.exp_value(c);
            }
        }
    }

    //// State::Value
    fn exp_value(&mut self, c: char) {
        match c {
            '{' => {
                self.z.push(Token::DictionaryStart);
                self.s.pop_and_push(State::Dictionary);
            }
            '[' => {
                self.z.push(Token::ListStart);
                self.s.pop_and_push(State::List);
            }
            '(' => {
                self.z.push(Token::TupleStart);
                self.s.pop_and_push(State::Tuple);
            }
            '$' => {
                self.z.push(Token::Dollar);
                self.s.pop_and_push(State::Variable);
            }
            '\x27' => {
                self.z.push(Token::SingleQuotation);
                self.s.pop_and_push(State::StrWrappedInSingleQuotationMarks);
            }
            '"' => {
                self.z.push(Token::DoubleQuotation);
                self.s.pop_and_push(State::StrWrappedInDoubleQuotationMarks);
            }
            _ => {
                self.s.pop_and_push(State::Undetermined);
                // bool | number | fn call chain | string
                self.exp_undetermined(c);
            }
        }
    }

    //// State::Dictionary
    fn exp_dict(&mut self, c: char) {
        match c {
            // stop
            '}' => {
                self.z.push(Token::DictionaryEnd);
                self.s.pop();
            }
            // next (pattern => value) group
            ',' => {
                self.z.push(Token::Comma);
                self.s.push(State::BoxPattern);
            }
            // default
            _ => {
                self.s.push(State::BoxPattern);
                self.exp_box_pattern(c);
            }
        }
    }

    //// State::Pattern
    fn exp_pattern(&mut self, c: char) {
        match c {
            // stop
            '\t' | '\x20' | '\r' | '\n' => {
                self.push_and_clear_if_t(Token::Pattern(self.t.iter().collect::<String>()));
                self.s.pop();
            }
            '=' if self.r.peek() == '>' => {
                self.push_and_clear_if_t(Token::Pattern(self.t.iter().collect::<String>()));
                self.r.skip(1);
                self.z.push(Token::Arrow);
                self.s.pop_and_push(State::BoxValue);
            }
            // legal char(generalized string && http path route pattern)
            // rfc3986
            '/' | 'A'..='Z' | 'a'..='z' | '0'..='9' | '.' | '*' | '?' | '&' | '_' | '~' | '-' | '=' => {
                self.t.push(c);
            }
            // illegal char in pattern
            _ => self.error(c),
        }
    }

    fn infer_from_str_like(&self, s: &str) -> Token {
        match s {
            "true" | "false" => Token::Boolean(s.into()),
            _ if Regex::new(r"^-?\d*(.\d+)?([Ee]-?\d+)?$").unwrap().is_match(s) => Token::Number(s.into()),
            _ => Token::String(s.into()),
        }
    }

    // fn-name | number | string
    // State::StrMaybe
    fn exp_undetermined(&mut self, c: char) {
        match c {
            // stop
            ',' | ')' | ']' | '}' => {
                self.push_and_clear_if_t(self.infer_from_str_like(self.t.iter().collect::<String>().as_str()));
                self.s.pop();
                self.exp_box_value(c);
            }
            '(' => {
                self.push_and_clear_if_t(Token::FnName(self.t.iter().collect::<String>()));
                self.z.push(Token::FnArgsStart);
                self.s.pop_and_push(State::Tuple); // (arg1, arg2 ..)
            }
            // recursive until true stop-char
            '\x20' | '\t' | '\r' | '\n' => {
                self.push_and_clear_if_t(self.infer_from_str_like(self.t.iter().collect::<String>().as_str()));
                let nxt = self.r.next();
                self.exp_undetermined(nxt);
            }
            //
            _ => self.t.push(c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t(s: &str) -> Vec<Token> {
        let mut reader = CharReader::new(BufReader::new(s.as_bytes()));
        Lexical::new(reader).tokenize()
    }

    #[test]
    fn test_dict1() {
        assert_eq!(
            t(r#"{a=b=>c}"#),
            vec![
                Token::DictionaryStart,
                Token::Pattern(String::from("a=b")),
                Token::Arrow,
                Token::String(String::from("c")),
                Token::DictionaryEnd
            ]
        );
    }
}
