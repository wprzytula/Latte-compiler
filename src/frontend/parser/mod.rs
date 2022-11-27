#![allow(warnings, unused)]

pub mod lattelexer;
pub mod lattelistener;
pub mod latteparser;
pub mod lattevisitor;

use antlr_rust::char_stream::CharStream;
use antlr_rust::error_listener::ErrorListener;
use antlr_rust::errors::ANTLRError;
use antlr_rust::interval_set::Interval;
use antlr_rust::parser::ParserNodeType;
use antlr_rust::recognizer::Recognizer;
use antlr_rust::token_stream::TokenStream;
use antlr_rust::{
    common_token_stream::CommonTokenStream, token_factory::CommonTokenFactory, InputStream,
};
use antlr_rust::{DefaultErrorStrategy, TokenSource};
use antlr_rust::{ErrorStrategy, Parser};
use better_any::{Tid, TidAble};
pub use lattelexer::LatteLexer;
pub use latteparser::LatteParser;
use std::cell::Cell;
use std::fs;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

use self::latteparser::ProgramContextAll;

static TF: CommonTokenFactory = CommonTokenFactory;

better_any::tid! { impl<'i,Ctx> TidAble<'i> for MyErrorStrategy<'i,Ctx> where Ctx: ParserNodeType<'i>}

pub struct MyErrorStrategy<'input, Ctx: ParserNodeType<'input>>(DefaultErrorStrategy<'input, Ctx>);

impl<'a, T: Parser<'a>> ErrorStrategy<'a, T> for MyErrorStrategy<'a, T::Node> {
    fn reset(&mut self, recognizer: &mut T) {
        self.0.reset(recognizer)
    }

    fn recover_inline(
        &mut self,
        recognizer: &mut T,
    ) -> Result<<T::TF as antlr_rust::token_factory::TokenFactory<'a>>::Tok, ANTLRError> {
        self.0.recover_inline(recognizer)
    }

    fn recover(&mut self, recognizer: &mut T, e: &ANTLRError) -> Result<(), ANTLRError> {
        self.0.recover(recognizer, e)
    }

    fn sync(&mut self, recognizer: &mut T) -> Result<(), ANTLRError> {
        self.0.sync(recognizer)
    }

    fn in_error_recovery_mode(&mut self, recognizer: &mut T) -> bool {
        self.0.in_error_recovery_mode(recognizer)
    }

    fn report_error(&mut self, recognizer: &mut T, e: &ANTLRError) {
        eprintln!("ERROR!");
        self.0.report_error(recognizer, e)
    }

    fn report_match(&mut self, recognizer: &mut T) {
        self.0.report_match(recognizer)
    }
}

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
        MyErrorStrategy<'_, latteparser::LatteParserContextType>,
    >,
    Rc<Cell<bool>>,
) {
    let input = fs::read_to_string(path).expect("ERROR\nSomething went wrong reading the file");
    let was_error = Rc::new(Cell::new(false));

    let mut lexer = LatteLexer::new_with_token_factory(
        InputStream::new_owned(input.clone().into_boxed_str()),
        &TF,
    );
    lexer.add_error_listener(Box::new(ReportingErrorListener::new(was_error.clone())));
    let x = lexer.input.as_ref().unwrap();
    let s: String = <InputStream<Box<str>> as CharStream<_>>::get_text(x, 0, 70);

    let tokens = CommonTokenStream::new(lexer);

    let strategy = MyErrorStrategy(DefaultErrorStrategy::new());
    let mut parser = LatteParser::with_strategy(tokens, strategy);
    parser.add_error_listener(Box::new(ReportingErrorListener::new(was_error.clone())));

    (parser, was_error)
}
