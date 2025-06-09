use std::collections::{ hash_map::Entry, HashMap };

use crate::lex::token::{ TokenKind, KeywordKind };

pub const KEYWORD_KIND_LIST: &[KeywordKind] = &[
    KeywordKind::FN,
    KeywordKind::LET,
    KeywordKind::I32,
    KeywordKind::I8,
    KeywordKind::RETURN,
    KeywordKind::PRINT,
    KeywordKind::VOID,
    KeywordKind::BOOL,
    KeywordKind::TRUE,
    KeywordKind::FALSE,
    KeywordKind::EXTERN,
    KeywordKind::IF,
    KeywordKind::ELSE,
];

pub struct KeywordMatcher {
    start_node: MatcherNode,
}

struct MatcherNode {
    pub children: HashMap<char, Box<MatcherNode>>,
    pub kind: Option<TokenKind>,
}

impl KeywordMatcher {
    pub fn new() -> Self {
        let mut start_node = MatcherNode {
            children: HashMap::new(),
            kind: None,
        };

        for keyword in KEYWORD_KIND_LIST {
            let mut last_node: *mut MatcherNode = &mut start_node;

            for c in keyword.spelling().chars() {
                let children = unsafe { &mut (*last_node).children };

                match children.entry(c) {
                    Entry::Occupied(child_node) => {
                        last_node = child_node.into_mut().as_mut();
                    }

                    Entry::Vacant(e) => {
                        last_node = e.insert(Box::new(MatcherNode {
                            children: HashMap::new(),
                            kind: None,
                        })).as_mut();
                    }
                }
            }

            unsafe { (*last_node).kind = Some(TokenKind::from(keyword.clone())); }
        }

        KeywordMatcher { start_node }
    }

    pub fn search_str(&self, s: &str) -> Option<TokenKind> {
        let mut curr = &self.start_node;

        for c in s.chars() {
            if let Some(node) = curr.children.get(&c) {
                curr = node;
            } 
            else {
                return None;
            }
        }

        curr.kind.clone()
    }
}
