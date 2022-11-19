#![allow(warnings, unused)]

pub mod lattelexer;
pub mod lattelistener;
pub mod latteparser;
pub mod lattevisitor;

use antlr_rust::char_stream::CharStream;
use antlr_rust::error_listener::ErrorListener;
use antlr_rust::errors::ANTLRError;
use antlr_rust::interval_set::Interval;
use antlr_rust::recognizer::Recognizer;
use antlr_rust::token_stream::TokenStream;
use antlr_rust::Parser;
use antlr_rust::{
    common_token_stream::CommonTokenStream, token_factory::CommonTokenFactory, InputStream,
};
use antlr_rust::{DefaultErrorStrategy, TokenSource};
pub use lattelexer::LatteLexer;
pub use latteparser::LatteParser;
use std::cell::{Cell, RefCell};
use std::fs;
use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;

use self::latteparser::ProgramContextAll;

static TF: CommonTokenFactory = CommonTokenFactory;

struct ReportingErrorListener(Rc<Cell<bool>>);
impl ReportingErrorListener {
    pub fn new(was_error: Rc<Cell<bool>>) -> Self {
        assert!(!was_error.get());
        Self(was_error)
    }
}
impl<'a, T: Recognizer<'a>> ErrorListener<'a, T> for ReportingErrorListener {
    fn syntax_error(
        &self,
        _recognizer: &T,
        _offending_symbol: Option<&<T::TF as antlr_rust::token_factory::TokenFactory<'a>>::Inner>,
        _line: isize,
        _column: isize,
        _msg: &str,
        _error: Option<&ANTLRError>,
    ) {
        self.0.replace(true);
    }
}

pub struct IntervalDisplayer(InputStream<Box<str>>);
impl IntervalDisplayer {
    pub fn display_interval(&self, interval: Interval) -> String {
        self.display_bounds(interval.a, interval.b)
    }

    pub fn display_bounds(&self, start: isize, stop: isize) -> String {
        <InputStream<Box<str>> as CharStream<_>>::get_text(&self.0, 0, 70)
    }
}

pub fn build_parser<'f, P: AsRef<Path>>(
    path: &P,
) -> (
    LatteParser<
        '_,
        CommonTokenStream<'_, LatteLexer<'_, InputStream<Box<str>>>>,
        DefaultErrorStrategy<'_, latteparser::LatteParserContextType>,
    >,
    Rc<Cell<bool>>,
    IntervalDisplayer,
)
/* (Result<Rc<ProgramContextAll<'f>>, ANTLRError>, Vec<&'f str>) */
{
    println!("{}", path.as_ref().to_str().unwrap());
    let input = fs::read_to_string(path).expect("Something went wrong reading the file");
    let was_error = Rc::new(Cell::new(false));

    let mut lexer = LatteLexer::new_with_token_factory(
        InputStream::new_owned(input.clone().into_boxed_str()),
        &TF,
    );
    // lexer.remove_error_listeners();
    lexer.add_error_listener(Box::new(ReportingErrorListener::new(was_error.clone())));
    let x = lexer.input.as_ref().unwrap();
    let s: String = <InputStream<Box<str>> as CharStream<_>>::get_text(x, 0, 70);
    // println!("{}", s);

    let tokens = CommonTokenStream::new(lexer);
    // println!("{}", tokens.get_all_text());

    let mut parser = LatteParser::new(tokens);
    // parser.remove_error_listeners();
    parser.add_error_listener(Box::new(ReportingErrorListener::new(was_error.clone())));

    let interval_displayer =
        IntervalDisplayer({ InputStream::new_owned(input.clone().into_boxed_str()) });
    (parser, was_error, interval_displayer)
}
