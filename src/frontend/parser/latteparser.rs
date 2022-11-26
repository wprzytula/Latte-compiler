// Generated from src/grammars/ext/Latte.g4 by ANTLR 4.8
#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(nonstandard_style)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_braces)]
use super::lattelistener::*;
use super::lattevisitor::*;
use antlr_rust::atn::{ATN, INVALID_ALT};
use antlr_rust::atn_deserializer::ATNDeserializer;
use antlr_rust::dfa::DFA;
use antlr_rust::error_strategy::{DefaultErrorStrategy, ErrorStrategy};
use antlr_rust::errors::*;
use antlr_rust::int_stream::EOF;
use antlr_rust::parser::{BaseParser, Parser, ParserNodeType, ParserRecog};
use antlr_rust::parser_atn_simulator::ParserATNSimulator;
use antlr_rust::parser_rule_context::{cast, cast_mut, BaseParserRuleContext, ParserRuleContext};
use antlr_rust::recognizer::{Actions, Recognizer};
use antlr_rust::rule_context::{BaseRuleContext, CustomRuleContext, RuleContext};
use antlr_rust::token::{OwningToken, Token, TOKEN_EOF};
use antlr_rust::token_factory::{CommonTokenFactory, TokenAware, TokenFactory};
use antlr_rust::token_stream::TokenStream;
use antlr_rust::tree::*;
use antlr_rust::vocabulary::{Vocabulary, VocabularyImpl};
use antlr_rust::PredictionContextCache;
use antlr_rust::TokenSource;

use antlr_rust::lazy_static;
use antlr_rust::{TidAble, TidExt};

use std::any::{Any, TypeId};
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::convert::TryFrom;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::Arc;

pub const T__0: isize = 1;
pub const T__1: isize = 2;
pub const T__2: isize = 3;
pub const T__3: isize = 4;
pub const T__4: isize = 5;
pub const T__5: isize = 6;
pub const T__6: isize = 7;
pub const T__7: isize = 8;
pub const T__8: isize = 9;
pub const T__9: isize = 10;
pub const T__10: isize = 11;
pub const T__11: isize = 12;
pub const T__12: isize = 13;
pub const T__13: isize = 14;
pub const T__14: isize = 15;
pub const T__15: isize = 16;
pub const T__16: isize = 17;
pub const T__17: isize = 18;
pub const T__18: isize = 19;
pub const T__19: isize = 20;
pub const T__20: isize = 21;
pub const T__21: isize = 22;
pub const T__22: isize = 23;
pub const T__23: isize = 24;
pub const T__24: isize = 25;
pub const T__25: isize = 26;
pub const T__26: isize = 27;
pub const T__27: isize = 28;
pub const T__28: isize = 29;
pub const T__29: isize = 30;
pub const T__30: isize = 31;
pub const T__31: isize = 32;
pub const T__32: isize = 33;
pub const T__33: isize = 34;
pub const T__34: isize = 35;
pub const T__35: isize = 36;
pub const T__36: isize = 37;
pub const T__37: isize = 38;
pub const T__38: isize = 39;
pub const T__39: isize = 40;
pub const T__40: isize = 41;
pub const T__41: isize = 42;
pub const T__42: isize = 43;
pub const COMMENT: isize = 44;
pub const MULTICOMMENT: isize = 45;
pub const INT: isize = 46;
pub const ID: isize = 47;
pub const WS: isize = 48;
pub const STR: isize = 49;
pub const RULE_program: usize = 0;
pub const RULE_topDef: usize = 1;
pub const RULE_funDef: usize = 2;
pub const RULE_params: usize = 3;
pub const RULE_param: usize = 4;
pub const RULE_classBlock: usize = 5;
pub const RULE_classItem: usize = 6;
pub const RULE_decl: usize = 7;
pub const RULE_items: usize = 8;
pub const RULE_item: usize = 9;
pub const RULE_block: usize = 10;
pub const RULE_stmt: usize = 11;
pub const RULE_lval: usize = 12;
pub const RULE_type_: usize = 13;
pub const RULE_nonvoid_type: usize = 14;
pub const RULE_newtype: usize = 15;
pub const RULE_expr: usize = 16;
pub const RULE_args: usize = 17;
pub const RULE_arg: usize = 18;
pub const RULE_addOp: usize = 19;
pub const RULE_mulOp: usize = 20;
pub const RULE_relOp: usize = 21;
pub const ruleNames: [&'static str; 22] = [
    "program",
    "topDef",
    "funDef",
    "params",
    "param",
    "classBlock",
    "classItem",
    "decl",
    "items",
    "item",
    "block",
    "stmt",
    "lval",
    "type_",
    "nonvoid_type",
    "newtype",
    "expr",
    "args",
    "arg",
    "addOp",
    "mulOp",
    "relOp",
];

pub const _LITERAL_NAMES: [Option<&'static str>; 44] = [
    None,
    Some("'class'"),
    Some("'extends'"),
    Some("'('"),
    Some("')'"),
    Some("','"),
    Some("'{'"),
    Some("'}'"),
    Some("';'"),
    Some("'='"),
    Some("'++'"),
    Some("'--'"),
    Some("'return'"),
    Some("'if'"),
    Some("'else'"),
    Some("'while'"),
    Some("'for'"),
    Some("':'"),
    Some("'.'"),
    Some("'['"),
    Some("']'"),
    Some("'void'"),
    Some("'int'"),
    Some("'string'"),
    Some("'boolean'"),
    Some("'[]'"),
    Some("'-'"),
    Some("'!'"),
    Some("'&&'"),
    Some("'||'"),
    Some("'true'"),
    Some("'false'"),
    Some("'null'"),
    Some("'new'"),
    Some("'+'"),
    Some("'*'"),
    Some("'/'"),
    Some("'%'"),
    Some("'<'"),
    Some("'<='"),
    Some("'>'"),
    Some("'>='"),
    Some("'=='"),
    Some("'!='"),
];
pub const _SYMBOLIC_NAMES: [Option<&'static str>; 50] = [
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    Some("COMMENT"),
    Some("MULTICOMMENT"),
    Some("INT"),
    Some("ID"),
    Some("WS"),
    Some("STR"),
];
lazy_static! {
    static ref _shared_context_cache: Arc<PredictionContextCache> =
        Arc::new(PredictionContextCache::new());
    static ref VOCABULARY: Box<dyn Vocabulary> = Box::new(VocabularyImpl::new(
        _LITERAL_NAMES.iter(),
        _SYMBOLIC_NAMES.iter(),
        None
    ));
}

type BaseParserType<'input, I> = BaseParser<
    'input,
    LatteParserExt<'input>,
    I,
    LatteParserContextType,
    dyn LatteListener<'input> + 'input,
>;

type TokenType<'input> = <LocalTokenFactory<'input> as TokenFactory<'input>>::Tok;
pub type LocalTokenFactory<'input> = CommonTokenFactory;

pub type LatteTreeWalker<'input, 'a> =
    ParseTreeWalker<'input, 'a, LatteParserContextType, dyn LatteListener<'input> + 'a>;

/// Parser for Latte grammar
pub struct LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    base: BaseParserType<'input, I>,
    interpreter: Arc<ParserATNSimulator>,
    _shared_context_cache: Box<PredictionContextCache>,
    pub err_handler: H,
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn get_serialized_atn() -> &'static str {
        _serializedATN
    }

    pub fn set_error_strategy(&mut self, strategy: H) {
        self.err_handler = strategy
    }

    pub fn with_strategy(input: I, strategy: H) -> Self {
        antlr_rust::recognizer::check_version("0", "3");
        let interpreter = Arc::new(ParserATNSimulator::new(
            _ATN.clone(),
            _decision_to_DFA.clone(),
            _shared_context_cache.clone(),
        ));
        Self {
            base: BaseParser::new_base_parser(
                input,
                Arc::clone(&interpreter),
                LatteParserExt {
                    _pd: Default::default(),
                },
            ),
            interpreter,
            _shared_context_cache: Box::new(PredictionContextCache::new()),
            err_handler: strategy,
        }
    }
}

type DynStrategy<'input, I> = Box<dyn ErrorStrategy<'input, BaseParserType<'input, I>> + 'input>;

impl<'input, I> LatteParser<'input, I, DynStrategy<'input, I>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
{
    pub fn with_dyn_strategy(input: I) -> Self {
        Self::with_strategy(input, Box::new(DefaultErrorStrategy::new()))
    }
}

impl<'input, I> LatteParser<'input, I, DefaultErrorStrategy<'input, LatteParserContextType>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
{
    pub fn new(input: I) -> Self {
        Self::with_strategy(input, DefaultErrorStrategy::new())
    }
}

/// Trait for monomorphized trait object that corresponds to the nodes of parse tree generated for LatteParser
pub trait LatteParserContext<'input>:
    for<'x> Listenable<dyn LatteListener<'input> + 'x>
    + for<'x> Visitable<dyn LatteVisitor<'input> + 'x>
    + ParserRuleContext<'input, TF = LocalTokenFactory<'input>, Ctx = LatteParserContextType>
{
}

antlr_rust::coerce_from! { 'input : LatteParserContext<'input> }

impl<'input, 'x, T> VisitableDyn<T> for dyn LatteParserContext<'input> + 'input
where
    T: LatteVisitor<'input> + 'x,
{
    fn accept_dyn(&self, visitor: &mut T) {
        self.accept(visitor as &mut (dyn LatteVisitor<'input> + 'x))
    }
}

impl<'input> LatteParserContext<'input> for TerminalNode<'input, LatteParserContextType> {}
impl<'input> LatteParserContext<'input> for ErrorNode<'input, LatteParserContextType> {}

antlr_rust::tid! { impl<'input> TidAble<'input> for dyn LatteParserContext<'input> + 'input }

antlr_rust::tid! { impl<'input> TidAble<'input> for dyn LatteListener<'input> + 'input }

pub struct LatteParserContextType;
antlr_rust::tid! {LatteParserContextType}

impl<'input> ParserNodeType<'input> for LatteParserContextType {
    type TF = LocalTokenFactory<'input>;
    type Type = dyn LatteParserContext<'input> + 'input;
}

impl<'input, I, H> Deref for LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    type Target = BaseParserType<'input, I>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'input, I, H> DerefMut for LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

pub struct LatteParserExt<'input> {
    _pd: PhantomData<&'input str>,
}

impl<'input> LatteParserExt<'input> {}
antlr_rust::tid! { LatteParserExt<'a> }

impl<'input> TokenAware<'input> for LatteParserExt<'input> {
    type TF = LocalTokenFactory<'input>;
}

impl<'input, I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>>
    ParserRecog<'input, BaseParserType<'input, I>> for LatteParserExt<'input>
{
}

impl<'input, I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>>
    Actions<'input, BaseParserType<'input, I>> for LatteParserExt<'input>
{
    fn get_grammar_file_name(&self) -> &str {
        "Latte.g4"
    }

    fn get_rule_names(&self) -> &[&str] {
        &ruleNames
    }

    fn get_vocabulary(&self) -> &dyn Vocabulary {
        &**VOCABULARY
    }
    fn sempred(
        _localctx: Option<&(dyn LatteParserContext<'input> + 'input)>,
        rule_index: isize,
        pred_index: isize,
        recog: &mut BaseParserType<'input, I>,
    ) -> bool {
        match rule_index {
            12 => LatteParser::<'input, I, _>::lval_sempred(
                _localctx.and_then(|x| x.downcast_ref()),
                pred_index,
                recog,
            ),
            14 => LatteParser::<'input, I, _>::nonvoid_type_sempred(
                _localctx.and_then(|x| x.downcast_ref()),
                pred_index,
                recog,
            ),
            16 => LatteParser::<'input, I, _>::expr_sempred(
                _localctx.and_then(|x| x.downcast_ref()),
                pred_index,
                recog,
            ),
            _ => true,
        }
    }
}

impl<'input, I> LatteParser<'input, I, DefaultErrorStrategy<'input, LatteParserContextType>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
{
    fn lval_sempred(
        _localctx: Option<&LvalContext<'input>>,
        pred_index: isize,
        recog: &mut <Self as Deref>::Target,
    ) -> bool {
        match pred_index {
            0 => recog.precpred(None, 2),
            1 => recog.precpred(None, 1),
            _ => true,
        }
    }
    fn nonvoid_type_sempred(
        _localctx: Option<&Nonvoid_typeContext<'input>>,
        pred_index: isize,
        recog: &mut <Self as Deref>::Target,
    ) -> bool {
        match pred_index {
            2 => recog.precpred(None, 2),
            _ => true,
        }
    }
    fn expr_sempred(
        _localctx: Option<&ExprContext<'input>>,
        pred_index: isize,
        recog: &mut <Self as Deref>::Target,
    ) -> bool {
        match pred_index {
            3 => recog.precpred(None, 17),
            4 => recog.precpred(None, 16),
            5 => recog.precpred(None, 15),
            6 => recog.precpred(None, 14),
            7 => recog.precpred(None, 13),
            8 => recog.precpred(None, 4),
            9 => recog.precpred(None, 3),
            10 => recog.precpred(None, 2),
            _ => true,
        }
    }
}
//------------------- program ----------------
pub type ProgramContextAll<'input> = ProgramContext<'input>;

pub type ProgramContext<'input> = BaseParserRuleContext<'input, ProgramContextExt<'input>>;

#[derive(Clone)]
pub struct ProgramContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ProgramContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ProgramContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_program(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_program(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ProgramContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_program(self);
    }
}

impl<'input> CustomRuleContext<'input> for ProgramContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_program
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_program }
}
antlr_rust::tid! {ProgramContextExt<'a>}

impl<'input> ProgramContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ProgramContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            ProgramContextExt { ph: PhantomData },
        ))
    }
}

pub trait ProgramContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ProgramContextExt<'input>>
{
    fn topDef_all(&self) -> Vec<Rc<TopDefContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn topDef(&self, i: usize) -> Option<Rc<TopDefContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> ProgramContextAttrs<'input> for ProgramContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn program(&mut self) -> Result<Rc<ProgramContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ProgramContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 0, RULE_program);
        let mut _localctx: Rc<ProgramContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(45);
                recog.err_handler.sync(&mut recog.base)?;
                _la = recog.base.input.la(1);
                loop {
                    {
                        {
                            /*InvokeRule topDef*/
                            recog.base.set_state(44);
                            recog.topDef()?;
                        }
                    }
                    recog.base.set_state(47);
                    recog.err_handler.sync(&mut recog.base)?;
                    _la = recog.base.input.la(1);
                    if !((((_la) & !0x3f) == 0
                        && ((1usize << _la)
                            & ((1usize << T__0)
                                | (1usize << T__20)
                                | (1usize << T__21)
                                | (1usize << T__22)
                                | (1usize << T__23)))
                            != 0)
                        || _la == ID)
                    {
                        break;
                    }
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- topDef ----------------
#[derive(Debug)]
pub enum TopDefContextAll<'input> {
    DerivClsContext(DerivClsContext<'input>),
    TopFnDefContext(TopFnDefContext<'input>),
    BaseClsContext(BaseClsContext<'input>),
    Error(TopDefContext<'input>),
}
antlr_rust::tid! {TopDefContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for TopDefContextAll<'input> {}

impl<'input> LatteParserContext<'input> for TopDefContextAll<'input> {}

impl<'input> Deref for TopDefContextAll<'input> {
    type Target = dyn TopDefContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use TopDefContextAll::*;
        match self {
            DerivClsContext(inner) => inner,
            TopFnDefContext(inner) => inner,
            BaseClsContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for TopDefContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for TopDefContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type TopDefContext<'input> = BaseParserRuleContext<'input, TopDefContextExt<'input>>;

#[derive(Clone)]
pub struct TopDefContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for TopDefContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for TopDefContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for TopDefContext<'input> {}

impl<'input> CustomRuleContext<'input> for TopDefContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_topDef
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_topDef }
}
antlr_rust::tid! {TopDefContextExt<'a>}

impl<'input> TopDefContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<TopDefContextAll<'input>> {
        Rc::new(TopDefContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                TopDefContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait TopDefContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<TopDefContextExt<'input>>
{
}

impl<'input> TopDefContextAttrs<'input> for TopDefContext<'input> {}

pub type DerivClsContext<'input> = BaseParserRuleContext<'input, DerivClsContextExt<'input>>;

pub trait DerivClsContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves all `TerminalNode`s corresponding to token ID in current rule
    fn ID_all(&self) -> Vec<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    /// Retrieves 'i's TerminalNode corresponding to token ID, starting from 0.
    /// Returns `None` if number of children corresponding to token ID is less or equal than `i`.
    fn ID(&self, i: usize) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, i)
    }
    fn classBlock(&self) -> Option<Rc<ClassBlockContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> DerivClsContextAttrs<'input> for DerivClsContext<'input> {}

pub struct DerivClsContextExt<'input> {
    base: TopDefContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {DerivClsContextExt<'a>}

impl<'input> LatteParserContext<'input> for DerivClsContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for DerivClsContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_DerivCls(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_DerivCls(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for DerivClsContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_DerivCls(self);
    }
}

impl<'input> CustomRuleContext<'input> for DerivClsContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_topDef
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_topDef }
}

impl<'input> Borrow<TopDefContextExt<'input>> for DerivClsContext<'input> {
    fn borrow(&self) -> &TopDefContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<TopDefContextExt<'input>> for DerivClsContext<'input> {
    fn borrow_mut(&mut self) -> &mut TopDefContextExt<'input> {
        &mut self.base
    }
}

impl<'input> TopDefContextAttrs<'input> for DerivClsContext<'input> {}

impl<'input> DerivClsContextExt<'input> {
    fn new(ctx: &dyn TopDefContextAttrs<'input>) -> Rc<TopDefContextAll<'input>> {
        Rc::new(TopDefContextAll::DerivClsContext(
            BaseParserRuleContext::copy_from(
                ctx,
                DerivClsContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type TopFnDefContext<'input> = BaseParserRuleContext<'input, TopFnDefContextExt<'input>>;

pub trait TopFnDefContextAttrs<'input>: LatteParserContext<'input> {
    fn funDef(&self) -> Option<Rc<FunDefContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> TopFnDefContextAttrs<'input> for TopFnDefContext<'input> {}

pub struct TopFnDefContextExt<'input> {
    base: TopDefContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {TopFnDefContextExt<'a>}

impl<'input> LatteParserContext<'input> for TopFnDefContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for TopFnDefContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_TopFnDef(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_TopFnDef(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for TopFnDefContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_TopFnDef(self);
    }
}

impl<'input> CustomRuleContext<'input> for TopFnDefContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_topDef
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_topDef }
}

impl<'input> Borrow<TopDefContextExt<'input>> for TopFnDefContext<'input> {
    fn borrow(&self) -> &TopDefContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<TopDefContextExt<'input>> for TopFnDefContext<'input> {
    fn borrow_mut(&mut self) -> &mut TopDefContextExt<'input> {
        &mut self.base
    }
}

impl<'input> TopDefContextAttrs<'input> for TopFnDefContext<'input> {}

impl<'input> TopFnDefContextExt<'input> {
    fn new(ctx: &dyn TopDefContextAttrs<'input>) -> Rc<TopDefContextAll<'input>> {
        Rc::new(TopDefContextAll::TopFnDefContext(
            BaseParserRuleContext::copy_from(
                ctx,
                TopFnDefContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type BaseClsContext<'input> = BaseParserRuleContext<'input, BaseClsContextExt<'input>>;

pub trait BaseClsContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
    fn classBlock(&self) -> Option<Rc<ClassBlockContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> BaseClsContextAttrs<'input> for BaseClsContext<'input> {}

pub struct BaseClsContextExt<'input> {
    base: TopDefContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {BaseClsContextExt<'a>}

impl<'input> LatteParserContext<'input> for BaseClsContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for BaseClsContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_BaseCls(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_BaseCls(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for BaseClsContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_BaseCls(self);
    }
}

impl<'input> CustomRuleContext<'input> for BaseClsContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_topDef
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_topDef }
}

impl<'input> Borrow<TopDefContextExt<'input>> for BaseClsContext<'input> {
    fn borrow(&self) -> &TopDefContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<TopDefContextExt<'input>> for BaseClsContext<'input> {
    fn borrow_mut(&mut self) -> &mut TopDefContextExt<'input> {
        &mut self.base
    }
}

impl<'input> TopDefContextAttrs<'input> for BaseClsContext<'input> {}

impl<'input> BaseClsContextExt<'input> {
    fn new(ctx: &dyn TopDefContextAttrs<'input>) -> Rc<TopDefContextAll<'input>> {
        Rc::new(TopDefContextAll::BaseClsContext(
            BaseParserRuleContext::copy_from(
                ctx,
                BaseClsContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn topDef(&mut self) -> Result<Rc<TopDefContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = TopDefContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 2, RULE_topDef);
        let mut _localctx: Rc<TopDefContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(58);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.interpreter.adaptive_predict(1, &mut recog.base)? {
                1 => {
                    let tmp = TopFnDefContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 1);
                    _localctx = tmp;
                    {
                        /*InvokeRule funDef*/
                        recog.base.set_state(49);
                        recog.funDef()?;
                    }
                }
                2 => {
                    let tmp = BaseClsContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 2);
                    _localctx = tmp;
                    {
                        recog.base.set_state(50);
                        recog.base.match_token(T__0, &mut recog.err_handler)?;

                        recog.base.set_state(51);
                        recog.base.match_token(ID, &mut recog.err_handler)?;

                        /*InvokeRule classBlock*/
                        recog.base.set_state(52);
                        recog.classBlock()?;
                    }
                }
                3 => {
                    let tmp = DerivClsContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 3);
                    _localctx = tmp;
                    {
                        recog.base.set_state(53);
                        recog.base.match_token(T__0, &mut recog.err_handler)?;

                        recog.base.set_state(54);
                        recog.base.match_token(ID, &mut recog.err_handler)?;

                        recog.base.set_state(55);
                        recog.base.match_token(T__1, &mut recog.err_handler)?;

                        recog.base.set_state(56);
                        recog.base.match_token(ID, &mut recog.err_handler)?;

                        /*InvokeRule classBlock*/
                        recog.base.set_state(57);
                        recog.classBlock()?;
                    }
                }

                _ => {}
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- funDef ----------------
pub type FunDefContextAll<'input> = FunDefContext<'input>;

pub type FunDefContext<'input> = BaseParserRuleContext<'input, FunDefContextExt<'input>>;

#[derive(Clone)]
pub struct FunDefContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for FunDefContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for FunDefContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_funDef(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_funDef(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for FunDefContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_funDef(self);
    }
}

impl<'input> CustomRuleContext<'input> for FunDefContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_funDef
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_funDef }
}
antlr_rust::tid! {FunDefContextExt<'a>}

impl<'input> FunDefContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<FunDefContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            FunDefContextExt { ph: PhantomData },
        ))
    }
}

pub trait FunDefContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<FunDefContextExt<'input>>
{
    fn type_(&self) -> Option<Rc<Type_ContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
    fn params(&self) -> Option<Rc<ParamsContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn block(&self) -> Option<Rc<BlockContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> FunDefContextAttrs<'input> for FunDefContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn funDef(&mut self) -> Result<Rc<FunDefContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = FunDefContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 4, RULE_funDef);
        let mut _localctx: Rc<FunDefContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                /*InvokeRule type_*/
                recog.base.set_state(60);
                recog.type_()?;

                recog.base.set_state(61);
                recog.base.match_token(ID, &mut recog.err_handler)?;

                recog.base.set_state(62);
                recog.base.match_token(T__2, &mut recog.err_handler)?;

                /*InvokeRule params*/
                recog.base.set_state(63);
                recog.params()?;

                recog.base.set_state(64);
                recog.base.match_token(T__3, &mut recog.err_handler)?;

                /*InvokeRule block*/
                recog.base.set_state(65);
                recog.block()?;
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- params ----------------
pub type ParamsContextAll<'input> = ParamsContext<'input>;

pub type ParamsContext<'input> = BaseParserRuleContext<'input, ParamsContextExt<'input>>;

#[derive(Clone)]
pub struct ParamsContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ParamsContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ParamsContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_params(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_params(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ParamsContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_params(self);
    }
}

impl<'input> CustomRuleContext<'input> for ParamsContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_params
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_params }
}
antlr_rust::tid! {ParamsContextExt<'a>}

impl<'input> ParamsContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ParamsContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            ParamsContextExt { ph: PhantomData },
        ))
    }
}

pub trait ParamsContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ParamsContextExt<'input>>
{
    fn param_all(&self) -> Vec<Rc<ParamContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn param(&self, i: usize) -> Option<Rc<ParamContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> ParamsContextAttrs<'input> for ParamsContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn params(&mut self) -> Result<Rc<ParamsContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ParamsContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 6, RULE_params);
        let mut _localctx: Rc<ParamsContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(76);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.base.input.la(1) {
                T__21 | T__22 | T__23 | ID => {
                    //recog.base.enter_outer_alt(_localctx.clone(), 1);
                    recog.base.enter_outer_alt(None, 1);
                    {
                        /*InvokeRule param*/
                        recog.base.set_state(67);
                        recog.param()?;

                        recog.base.set_state(72);
                        recog.err_handler.sync(&mut recog.base)?;
                        _la = recog.base.input.la(1);
                        while _la == T__4 {
                            {
                                {
                                    recog.base.set_state(68);
                                    recog.base.match_token(T__4, &mut recog.err_handler)?;

                                    /*InvokeRule param*/
                                    recog.base.set_state(69);
                                    recog.param()?;
                                }
                            }
                            recog.base.set_state(74);
                            recog.err_handler.sync(&mut recog.base)?;
                            _la = recog.base.input.la(1);
                        }
                    }
                }

                T__3 => {
                    //recog.base.enter_outer_alt(_localctx.clone(), 2);
                    recog.base.enter_outer_alt(None, 2);
                    {}
                }

                _ => Err(ANTLRError::NoAltError(NoViableAltError::new(
                    &mut recog.base,
                )))?,
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- param ----------------
pub type ParamContextAll<'input> = ParamContext<'input>;

pub type ParamContext<'input> = BaseParserRuleContext<'input, ParamContextExt<'input>>;

#[derive(Clone)]
pub struct ParamContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ParamContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ParamContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_param(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_param(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ParamContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_param(self);
    }
}

impl<'input> CustomRuleContext<'input> for ParamContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_param
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_param }
}
antlr_rust::tid! {ParamContextExt<'a>}

impl<'input> ParamContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ParamContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            ParamContextExt { ph: PhantomData },
        ))
    }
}

pub trait ParamContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ParamContextExt<'input>>
{
    fn nonvoid_type(&self) -> Option<Rc<Nonvoid_typeContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> ParamContextAttrs<'input> for ParamContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn param(&mut self) -> Result<Rc<ParamContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ParamContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 8, RULE_param);
        let mut _localctx: Rc<ParamContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                /*InvokeRule nonvoid_type*/
                recog.base.set_state(78);
                recog.nonvoid_type_rec(0)?;

                recog.base.set_state(79);
                recog.base.match_token(ID, &mut recog.err_handler)?;
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- classBlock ----------------
pub type ClassBlockContextAll<'input> = ClassBlockContext<'input>;

pub type ClassBlockContext<'input> = BaseParserRuleContext<'input, ClassBlockContextExt<'input>>;

#[derive(Clone)]
pub struct ClassBlockContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ClassBlockContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ClassBlockContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_classBlock(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_classBlock(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ClassBlockContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_classBlock(self);
    }
}

impl<'input> CustomRuleContext<'input> for ClassBlockContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_classBlock
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_classBlock }
}
antlr_rust::tid! {ClassBlockContextExt<'a>}

impl<'input> ClassBlockContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ClassBlockContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            ClassBlockContextExt { ph: PhantomData },
        ))
    }
}

pub trait ClassBlockContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ClassBlockContextExt<'input>>
{
    fn classItem_all(&self) -> Vec<Rc<ClassItemContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn classItem(&self, i: usize) -> Option<Rc<ClassItemContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> ClassBlockContextAttrs<'input> for ClassBlockContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn classBlock(&mut self) -> Result<Rc<ClassBlockContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ClassBlockContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog
            .base
            .enter_rule(_localctx.clone(), 10, RULE_classBlock);
        let mut _localctx: Rc<ClassBlockContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(81);
                recog.base.match_token(T__5, &mut recog.err_handler)?;

                recog.base.set_state(85);
                recog.err_handler.sync(&mut recog.base)?;
                _la = recog.base.input.la(1);
                while (((_la - 21) & !0x3f) == 0
                    && ((1usize << (_la - 21))
                        & ((1usize << (T__20 - 21))
                            | (1usize << (T__21 - 21))
                            | (1usize << (T__22 - 21))
                            | (1usize << (T__23 - 21))
                            | (1usize << (ID - 21))))
                        != 0)
                {
                    {
                        {
                            /*InvokeRule classItem*/
                            recog.base.set_state(82);
                            recog.classItem()?;
                        }
                    }
                    recog.base.set_state(87);
                    recog.err_handler.sync(&mut recog.base)?;
                    _la = recog.base.input.la(1);
                }
                recog.base.set_state(88);
                recog.base.match_token(T__6, &mut recog.err_handler)?;
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- classItem ----------------
#[derive(Debug)]
pub enum ClassItemContextAll<'input> {
    FieldContext(FieldContext<'input>),
    MethodContext(MethodContext<'input>),
    Error(ClassItemContext<'input>),
}
antlr_rust::tid! {ClassItemContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for ClassItemContextAll<'input> {}

impl<'input> LatteParserContext<'input> for ClassItemContextAll<'input> {}

impl<'input> Deref for ClassItemContextAll<'input> {
    type Target = dyn ClassItemContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use ClassItemContextAll::*;
        match self {
            FieldContext(inner) => inner,
            MethodContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ClassItemContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ClassItemContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type ClassItemContext<'input> = BaseParserRuleContext<'input, ClassItemContextExt<'input>>;

#[derive(Clone)]
pub struct ClassItemContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ClassItemContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ClassItemContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ClassItemContext<'input> {}

impl<'input> CustomRuleContext<'input> for ClassItemContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_classItem
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_classItem }
}
antlr_rust::tid! {ClassItemContextExt<'a>}

impl<'input> ClassItemContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ClassItemContextAll<'input>> {
        Rc::new(ClassItemContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                ClassItemContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait ClassItemContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ClassItemContextExt<'input>>
{
}

impl<'input> ClassItemContextAttrs<'input> for ClassItemContext<'input> {}

pub type FieldContext<'input> = BaseParserRuleContext<'input, FieldContextExt<'input>>;

pub trait FieldContextAttrs<'input>: LatteParserContext<'input> {
    fn decl(&self) -> Option<Rc<DeclContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> FieldContextAttrs<'input> for FieldContext<'input> {}

pub struct FieldContextExt<'input> {
    base: ClassItemContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {FieldContextExt<'a>}

impl<'input> LatteParserContext<'input> for FieldContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for FieldContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Field(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Field(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for FieldContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Field(self);
    }
}

impl<'input> CustomRuleContext<'input> for FieldContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_classItem
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_classItem }
}

impl<'input> Borrow<ClassItemContextExt<'input>> for FieldContext<'input> {
    fn borrow(&self) -> &ClassItemContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ClassItemContextExt<'input>> for FieldContext<'input> {
    fn borrow_mut(&mut self) -> &mut ClassItemContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ClassItemContextAttrs<'input> for FieldContext<'input> {}

impl<'input> FieldContextExt<'input> {
    fn new(ctx: &dyn ClassItemContextAttrs<'input>) -> Rc<ClassItemContextAll<'input>> {
        Rc::new(ClassItemContextAll::FieldContext(
            BaseParserRuleContext::copy_from(
                ctx,
                FieldContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type MethodContext<'input> = BaseParserRuleContext<'input, MethodContextExt<'input>>;

pub trait MethodContextAttrs<'input>: LatteParserContext<'input> {
    fn funDef(&self) -> Option<Rc<FunDefContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> MethodContextAttrs<'input> for MethodContext<'input> {}

pub struct MethodContextExt<'input> {
    base: ClassItemContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {MethodContextExt<'a>}

impl<'input> LatteParserContext<'input> for MethodContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for MethodContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Method(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Method(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for MethodContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Method(self);
    }
}

impl<'input> CustomRuleContext<'input> for MethodContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_classItem
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_classItem }
}

impl<'input> Borrow<ClassItemContextExt<'input>> for MethodContext<'input> {
    fn borrow(&self) -> &ClassItemContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ClassItemContextExt<'input>> for MethodContext<'input> {
    fn borrow_mut(&mut self) -> &mut ClassItemContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ClassItemContextAttrs<'input> for MethodContext<'input> {}

impl<'input> MethodContextExt<'input> {
    fn new(ctx: &dyn ClassItemContextAttrs<'input>) -> Rc<ClassItemContextAll<'input>> {
        Rc::new(ClassItemContextAll::MethodContext(
            BaseParserRuleContext::copy_from(
                ctx,
                MethodContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn classItem(&mut self) -> Result<Rc<ClassItemContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ClassItemContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 12, RULE_classItem);
        let mut _localctx: Rc<ClassItemContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(92);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.interpreter.adaptive_predict(5, &mut recog.base)? {
                1 => {
                    let tmp = FieldContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 1);
                    _localctx = tmp;
                    {
                        /*InvokeRule decl*/
                        recog.base.set_state(90);
                        recog.decl()?;
                    }
                }
                2 => {
                    let tmp = MethodContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 2);
                    _localctx = tmp;
                    {
                        /*InvokeRule funDef*/
                        recog.base.set_state(91);
                        recog.funDef()?;
                    }
                }

                _ => {}
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- decl ----------------
pub type DeclContextAll<'input> = DeclContext<'input>;

pub type DeclContext<'input> = BaseParserRuleContext<'input, DeclContextExt<'input>>;

#[derive(Clone)]
pub struct DeclContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for DeclContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for DeclContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_decl(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_decl(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for DeclContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_decl(self);
    }
}

impl<'input> CustomRuleContext<'input> for DeclContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_decl
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_decl }
}
antlr_rust::tid! {DeclContextExt<'a>}

impl<'input> DeclContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<DeclContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            DeclContextExt { ph: PhantomData },
        ))
    }
}

pub trait DeclContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<DeclContextExt<'input>>
{
    fn nonvoid_type(&self) -> Option<Rc<Nonvoid_typeContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn items(&self) -> Option<Rc<ItemsContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> DeclContextAttrs<'input> for DeclContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn decl(&mut self) -> Result<Rc<DeclContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = DeclContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 14, RULE_decl);
        let mut _localctx: Rc<DeclContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                /*InvokeRule nonvoid_type*/
                recog.base.set_state(94);
                recog.nonvoid_type_rec(0)?;

                /*InvokeRule items*/
                recog.base.set_state(95);
                recog.items()?;

                recog.base.set_state(96);
                recog.base.match_token(T__7, &mut recog.err_handler)?;
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- items ----------------
pub type ItemsContextAll<'input> = ItemsContext<'input>;

pub type ItemsContext<'input> = BaseParserRuleContext<'input, ItemsContextExt<'input>>;

#[derive(Clone)]
pub struct ItemsContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ItemsContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ItemsContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_items(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_items(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ItemsContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_items(self);
    }
}

impl<'input> CustomRuleContext<'input> for ItemsContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_items
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_items }
}
antlr_rust::tid! {ItemsContextExt<'a>}

impl<'input> ItemsContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ItemsContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            ItemsContextExt { ph: PhantomData },
        ))
    }
}

pub trait ItemsContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ItemsContextExt<'input>>
{
    fn item_all(&self) -> Vec<Rc<ItemContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn item(&self, i: usize) -> Option<Rc<ItemContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> ItemsContextAttrs<'input> for ItemsContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn items(&mut self) -> Result<Rc<ItemsContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ItemsContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 16, RULE_items);
        let mut _localctx: Rc<ItemsContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                /*InvokeRule item*/
                recog.base.set_state(98);
                recog.item()?;

                recog.base.set_state(103);
                recog.err_handler.sync(&mut recog.base)?;
                _la = recog.base.input.la(1);
                while _la == T__4 {
                    {
                        {
                            recog.base.set_state(99);
                            recog.base.match_token(T__4, &mut recog.err_handler)?;

                            /*InvokeRule item*/
                            recog.base.set_state(100);
                            recog.item()?;
                        }
                    }
                    recog.base.set_state(105);
                    recog.err_handler.sync(&mut recog.base)?;
                    _la = recog.base.input.la(1);
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- item ----------------
#[derive(Debug)]
pub enum ItemContextAll<'input> {
    DeclItemUninitContext(DeclItemUninitContext<'input>),
    DeclItemInitContext(DeclItemInitContext<'input>),
    Error(ItemContext<'input>),
}
antlr_rust::tid! {ItemContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for ItemContextAll<'input> {}

impl<'input> LatteParserContext<'input> for ItemContextAll<'input> {}

impl<'input> Deref for ItemContextAll<'input> {
    type Target = dyn ItemContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use ItemContextAll::*;
        match self {
            DeclItemUninitContext(inner) => inner,
            DeclItemInitContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ItemContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ItemContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type ItemContext<'input> = BaseParserRuleContext<'input, ItemContextExt<'input>>;

#[derive(Clone)]
pub struct ItemContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ItemContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ItemContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ItemContext<'input> {}

impl<'input> CustomRuleContext<'input> for ItemContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_item
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_item }
}
antlr_rust::tid! {ItemContextExt<'a>}

impl<'input> ItemContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ItemContextAll<'input>> {
        Rc::new(ItemContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                ItemContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait ItemContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ItemContextExt<'input>>
{
}

impl<'input> ItemContextAttrs<'input> for ItemContext<'input> {}

pub type DeclItemUninitContext<'input> =
    BaseParserRuleContext<'input, DeclItemUninitContextExt<'input>>;

pub trait DeclItemUninitContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> DeclItemUninitContextAttrs<'input> for DeclItemUninitContext<'input> {}

pub struct DeclItemUninitContextExt<'input> {
    base: ItemContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {DeclItemUninitContextExt<'a>}

impl<'input> LatteParserContext<'input> for DeclItemUninitContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for DeclItemUninitContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_DeclItemUninit(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_DeclItemUninit(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for DeclItemUninitContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_DeclItemUninit(self);
    }
}

impl<'input> CustomRuleContext<'input> for DeclItemUninitContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_item
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_item }
}

impl<'input> Borrow<ItemContextExt<'input>> for DeclItemUninitContext<'input> {
    fn borrow(&self) -> &ItemContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ItemContextExt<'input>> for DeclItemUninitContext<'input> {
    fn borrow_mut(&mut self) -> &mut ItemContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ItemContextAttrs<'input> for DeclItemUninitContext<'input> {}

impl<'input> DeclItemUninitContextExt<'input> {
    fn new(ctx: &dyn ItemContextAttrs<'input>) -> Rc<ItemContextAll<'input>> {
        Rc::new(ItemContextAll::DeclItemUninitContext(
            BaseParserRuleContext::copy_from(
                ctx,
                DeclItemUninitContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type DeclItemInitContext<'input> =
    BaseParserRuleContext<'input, DeclItemInitContextExt<'input>>;

pub trait DeclItemInitContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> DeclItemInitContextAttrs<'input> for DeclItemInitContext<'input> {}

pub struct DeclItemInitContextExt<'input> {
    base: ItemContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {DeclItemInitContextExt<'a>}

impl<'input> LatteParserContext<'input> for DeclItemInitContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for DeclItemInitContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_DeclItemInit(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_DeclItemInit(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for DeclItemInitContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_DeclItemInit(self);
    }
}

impl<'input> CustomRuleContext<'input> for DeclItemInitContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_item
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_item }
}

impl<'input> Borrow<ItemContextExt<'input>> for DeclItemInitContext<'input> {
    fn borrow(&self) -> &ItemContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ItemContextExt<'input>> for DeclItemInitContext<'input> {
    fn borrow_mut(&mut self) -> &mut ItemContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ItemContextAttrs<'input> for DeclItemInitContext<'input> {}

impl<'input> DeclItemInitContextExt<'input> {
    fn new(ctx: &dyn ItemContextAttrs<'input>) -> Rc<ItemContextAll<'input>> {
        Rc::new(ItemContextAll::DeclItemInitContext(
            BaseParserRuleContext::copy_from(
                ctx,
                DeclItemInitContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn item(&mut self) -> Result<Rc<ItemContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ItemContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 18, RULE_item);
        let mut _localctx: Rc<ItemContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(110);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.interpreter.adaptive_predict(7, &mut recog.base)? {
                1 => {
                    let tmp = DeclItemUninitContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 1);
                    _localctx = tmp;
                    {
                        recog.base.set_state(106);
                        recog.base.match_token(ID, &mut recog.err_handler)?;
                    }
                }
                2 => {
                    let tmp = DeclItemInitContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 2);
                    _localctx = tmp;
                    {
                        recog.base.set_state(107);
                        recog.base.match_token(ID, &mut recog.err_handler)?;

                        recog.base.set_state(108);
                        recog.base.match_token(T__8, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(109);
                        recog.expr_rec(0)?;
                    }
                }

                _ => {}
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- block ----------------
pub type BlockContextAll<'input> = BlockContext<'input>;

pub type BlockContext<'input> = BaseParserRuleContext<'input, BlockContextExt<'input>>;

#[derive(Clone)]
pub struct BlockContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for BlockContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for BlockContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_block(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_block(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for BlockContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_block(self);
    }
}

impl<'input> CustomRuleContext<'input> for BlockContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_block
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_block }
}
antlr_rust::tid! {BlockContextExt<'a>}

impl<'input> BlockContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<BlockContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            BlockContextExt { ph: PhantomData },
        ))
    }
}

pub trait BlockContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<BlockContextExt<'input>>
{
    fn stmt_all(&self) -> Vec<Rc<StmtContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn stmt(&self, i: usize) -> Option<Rc<StmtContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> BlockContextAttrs<'input> for BlockContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn block(&mut self) -> Result<Rc<BlockContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = BlockContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 20, RULE_block);
        let mut _localctx: Rc<BlockContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(112);
                recog.base.match_token(T__5, &mut recog.err_handler)?;

                recog.base.set_state(116);
                recog.err_handler.sync(&mut recog.base)?;
                _la = recog.base.input.la(1);
                while (((_la) & !0x3f) == 0
                    && ((1usize << _la)
                        & ((1usize << T__2)
                            | (1usize << T__5)
                            | (1usize << T__7)
                            | (1usize << T__11)
                            | (1usize << T__12)
                            | (1usize << T__14)
                            | (1usize << T__15)
                            | (1usize << T__21)
                            | (1usize << T__22)
                            | (1usize << T__23)
                            | (1usize << T__25)
                            | (1usize << T__26)
                            | (1usize << T__29)
                            | (1usize << T__30)))
                        != 0)
                    || (((_la - 33) & !0x3f) == 0
                        && ((1usize << (_la - 33))
                            & ((1usize << (T__32 - 33))
                                | (1usize << (INT - 33))
                                | (1usize << (ID - 33))
                                | (1usize << (STR - 33))))
                            != 0)
                {
                    {
                        {
                            /*InvokeRule stmt*/
                            recog.base.set_state(113);
                            recog.stmt()?;
                        }
                    }
                    recog.base.set_state(118);
                    recog.err_handler.sync(&mut recog.base)?;
                    _la = recog.base.input.la(1);
                }
                recog.base.set_state(119);
                recog.base.match_token(T__6, &mut recog.err_handler)?;
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- stmt ----------------
#[derive(Debug)]
pub enum StmtContextAll<'input> {
    AssContext(AssContext<'input>),
    RetContext(RetContext<'input>),
    CondContext(CondContext<'input>),
    CondElseContext(CondElseContext<'input>),
    VRetContext(VRetContext<'input>),
    BlockStmtContext(BlockStmtContext<'input>),
    ForContext(ForContext<'input>),
    WhileContext(WhileContext<'input>),
    SExpContext(SExpContext<'input>),
    DecrContext(DecrContext<'input>),
    EmptyContext(EmptyContext<'input>),
    VarDeclContext(VarDeclContext<'input>),
    IncrContext(IncrContext<'input>),
    Error(StmtContext<'input>),
}
antlr_rust::tid! {StmtContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for StmtContextAll<'input> {}

impl<'input> LatteParserContext<'input> for StmtContextAll<'input> {}

impl<'input> Deref for StmtContextAll<'input> {
    type Target = dyn StmtContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use StmtContextAll::*;
        match self {
            AssContext(inner) => inner,
            RetContext(inner) => inner,
            CondContext(inner) => inner,
            CondElseContext(inner) => inner,
            VRetContext(inner) => inner,
            BlockStmtContext(inner) => inner,
            ForContext(inner) => inner,
            WhileContext(inner) => inner,
            SExpContext(inner) => inner,
            DecrContext(inner) => inner,
            EmptyContext(inner) => inner,
            VarDeclContext(inner) => inner,
            IncrContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for StmtContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for StmtContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type StmtContext<'input> = BaseParserRuleContext<'input, StmtContextExt<'input>>;

#[derive(Clone)]
pub struct StmtContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for StmtContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for StmtContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for StmtContext<'input> {}

impl<'input> CustomRuleContext<'input> for StmtContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}
antlr_rust::tid! {StmtContextExt<'a>}

impl<'input> StmtContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                StmtContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait StmtContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<StmtContextExt<'input>>
{
}

impl<'input> StmtContextAttrs<'input> for StmtContext<'input> {}

pub type AssContext<'input> = BaseParserRuleContext<'input, AssContextExt<'input>>;

pub trait AssContextAttrs<'input>: LatteParserContext<'input> {
    fn lval(&self) -> Option<Rc<LvalContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> AssContextAttrs<'input> for AssContext<'input> {}

pub struct AssContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {AssContextExt<'a>}

impl<'input> LatteParserContext<'input> for AssContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for AssContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Ass(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Ass(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for AssContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Ass(self);
    }
}

impl<'input> CustomRuleContext<'input> for AssContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for AssContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for AssContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for AssContext<'input> {}

impl<'input> AssContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::AssContext(
            BaseParserRuleContext::copy_from(
                ctx,
                AssContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type RetContext<'input> = BaseParserRuleContext<'input, RetContextExt<'input>>;

pub trait RetContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> RetContextAttrs<'input> for RetContext<'input> {}

pub struct RetContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {RetContextExt<'a>}

impl<'input> LatteParserContext<'input> for RetContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for RetContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Ret(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Ret(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for RetContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Ret(self);
    }
}

impl<'input> CustomRuleContext<'input> for RetContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for RetContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for RetContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for RetContext<'input> {}

impl<'input> RetContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::RetContext(
            BaseParserRuleContext::copy_from(
                ctx,
                RetContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type CondContext<'input> = BaseParserRuleContext<'input, CondContextExt<'input>>;

pub trait CondContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn stmt(&self) -> Option<Rc<StmtContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> CondContextAttrs<'input> for CondContext<'input> {}

pub struct CondContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {CondContextExt<'a>}

impl<'input> LatteParserContext<'input> for CondContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for CondContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Cond(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Cond(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for CondContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Cond(self);
    }
}

impl<'input> CustomRuleContext<'input> for CondContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for CondContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for CondContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for CondContext<'input> {}

impl<'input> CondContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::CondContext(
            BaseParserRuleContext::copy_from(
                ctx,
                CondContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type CondElseContext<'input> = BaseParserRuleContext<'input, CondElseContextExt<'input>>;

pub trait CondElseContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn stmt_all(&self) -> Vec<Rc<StmtContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn stmt(&self, i: usize) -> Option<Rc<StmtContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> CondElseContextAttrs<'input> for CondElseContext<'input> {}

pub struct CondElseContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {CondElseContextExt<'a>}

impl<'input> LatteParserContext<'input> for CondElseContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for CondElseContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_CondElse(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_CondElse(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for CondElseContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_CondElse(self);
    }
}

impl<'input> CustomRuleContext<'input> for CondElseContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for CondElseContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for CondElseContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for CondElseContext<'input> {}

impl<'input> CondElseContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::CondElseContext(
            BaseParserRuleContext::copy_from(
                ctx,
                CondElseContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type VRetContext<'input> = BaseParserRuleContext<'input, VRetContextExt<'input>>;

pub trait VRetContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> VRetContextAttrs<'input> for VRetContext<'input> {}

pub struct VRetContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {VRetContextExt<'a>}

impl<'input> LatteParserContext<'input> for VRetContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for VRetContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_VRet(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_VRet(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for VRetContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_VRet(self);
    }
}

impl<'input> CustomRuleContext<'input> for VRetContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for VRetContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for VRetContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for VRetContext<'input> {}

impl<'input> VRetContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::VRetContext(
            BaseParserRuleContext::copy_from(
                ctx,
                VRetContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type BlockStmtContext<'input> = BaseParserRuleContext<'input, BlockStmtContextExt<'input>>;

pub trait BlockStmtContextAttrs<'input>: LatteParserContext<'input> {
    fn block(&self) -> Option<Rc<BlockContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> BlockStmtContextAttrs<'input> for BlockStmtContext<'input> {}

pub struct BlockStmtContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {BlockStmtContextExt<'a>}

impl<'input> LatteParserContext<'input> for BlockStmtContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for BlockStmtContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_BlockStmt(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_BlockStmt(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for BlockStmtContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_BlockStmt(self);
    }
}

impl<'input> CustomRuleContext<'input> for BlockStmtContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for BlockStmtContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for BlockStmtContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for BlockStmtContext<'input> {}

impl<'input> BlockStmtContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::BlockStmtContext(
            BaseParserRuleContext::copy_from(
                ctx,
                BlockStmtContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type ForContext<'input> = BaseParserRuleContext<'input, ForContextExt<'input>>;

pub trait ForContextAttrs<'input>: LatteParserContext<'input> {
    fn nonvoid_type(&self) -> Option<Rc<Nonvoid_typeContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn stmt(&self) -> Option<Rc<StmtContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> ForContextAttrs<'input> for ForContext<'input> {}

pub struct ForContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {ForContextExt<'a>}

impl<'input> LatteParserContext<'input> for ForContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ForContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_For(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_For(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ForContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_For(self);
    }
}

impl<'input> CustomRuleContext<'input> for ForContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for ForContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for ForContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for ForContext<'input> {}

impl<'input> ForContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::ForContext(
            BaseParserRuleContext::copy_from(
                ctx,
                ForContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type WhileContext<'input> = BaseParserRuleContext<'input, WhileContextExt<'input>>;

pub trait WhileContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn stmt(&self) -> Option<Rc<StmtContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> WhileContextAttrs<'input> for WhileContext<'input> {}

pub struct WhileContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {WhileContextExt<'a>}

impl<'input> LatteParserContext<'input> for WhileContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for WhileContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_While(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_While(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for WhileContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_While(self);
    }
}

impl<'input> CustomRuleContext<'input> for WhileContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for WhileContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for WhileContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for WhileContext<'input> {}

impl<'input> WhileContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::WhileContext(
            BaseParserRuleContext::copy_from(
                ctx,
                WhileContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type SExpContext<'input> = BaseParserRuleContext<'input, SExpContextExt<'input>>;

pub trait SExpContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> SExpContextAttrs<'input> for SExpContext<'input> {}

pub struct SExpContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {SExpContextExt<'a>}

impl<'input> LatteParserContext<'input> for SExpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for SExpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_SExp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_SExp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for SExpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_SExp(self);
    }
}

impl<'input> CustomRuleContext<'input> for SExpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for SExpContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for SExpContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for SExpContext<'input> {}

impl<'input> SExpContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::SExpContext(
            BaseParserRuleContext::copy_from(
                ctx,
                SExpContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type DecrContext<'input> = BaseParserRuleContext<'input, DecrContextExt<'input>>;

pub trait DecrContextAttrs<'input>: LatteParserContext<'input> {
    fn lval(&self) -> Option<Rc<LvalContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> DecrContextAttrs<'input> for DecrContext<'input> {}

pub struct DecrContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {DecrContextExt<'a>}

impl<'input> LatteParserContext<'input> for DecrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for DecrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Decr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Decr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for DecrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Decr(self);
    }
}

impl<'input> CustomRuleContext<'input> for DecrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for DecrContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for DecrContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for DecrContext<'input> {}

impl<'input> DecrContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::DecrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                DecrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EmptyContext<'input> = BaseParserRuleContext<'input, EmptyContextExt<'input>>;

pub trait EmptyContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> EmptyContextAttrs<'input> for EmptyContext<'input> {}

pub struct EmptyContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EmptyContextExt<'a>}

impl<'input> LatteParserContext<'input> for EmptyContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EmptyContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Empty(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Empty(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EmptyContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Empty(self);
    }
}

impl<'input> CustomRuleContext<'input> for EmptyContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for EmptyContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for EmptyContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for EmptyContext<'input> {}

impl<'input> EmptyContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::EmptyContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EmptyContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type VarDeclContext<'input> = BaseParserRuleContext<'input, VarDeclContextExt<'input>>;

pub trait VarDeclContextAttrs<'input>: LatteParserContext<'input> {
    fn decl(&self) -> Option<Rc<DeclContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> VarDeclContextAttrs<'input> for VarDeclContext<'input> {}

pub struct VarDeclContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {VarDeclContextExt<'a>}

impl<'input> LatteParserContext<'input> for VarDeclContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for VarDeclContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_VarDecl(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_VarDecl(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for VarDeclContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_VarDecl(self);
    }
}

impl<'input> CustomRuleContext<'input> for VarDeclContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for VarDeclContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for VarDeclContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for VarDeclContext<'input> {}

impl<'input> VarDeclContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::VarDeclContext(
            BaseParserRuleContext::copy_from(
                ctx,
                VarDeclContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type IncrContext<'input> = BaseParserRuleContext<'input, IncrContextExt<'input>>;

pub trait IncrContextAttrs<'input>: LatteParserContext<'input> {
    fn lval(&self) -> Option<Rc<LvalContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> IncrContextAttrs<'input> for IncrContext<'input> {}

pub struct IncrContextExt<'input> {
    base: StmtContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {IncrContextExt<'a>}

impl<'input> LatteParserContext<'input> for IncrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for IncrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Incr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Incr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for IncrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Incr(self);
    }
}

impl<'input> CustomRuleContext<'input> for IncrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_stmt
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_stmt }
}

impl<'input> Borrow<StmtContextExt<'input>> for IncrContext<'input> {
    fn borrow(&self) -> &StmtContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<StmtContextExt<'input>> for IncrContext<'input> {
    fn borrow_mut(&mut self) -> &mut StmtContextExt<'input> {
        &mut self.base
    }
}

impl<'input> StmtContextAttrs<'input> for IncrContext<'input> {}

impl<'input> IncrContextExt<'input> {
    fn new(ctx: &dyn StmtContextAttrs<'input>) -> Rc<StmtContextAll<'input>> {
        Rc::new(StmtContextAll::IncrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                IncrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn stmt(&mut self) -> Result<Rc<StmtContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = StmtContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 22, RULE_stmt);
        let mut _localctx: Rc<StmtContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(175);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.interpreter.adaptive_predict(9, &mut recog.base)? {
                1 => {
                    let tmp = EmptyContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 1);
                    _localctx = tmp;
                    {
                        recog.base.set_state(121);
                        recog.base.match_token(T__7, &mut recog.err_handler)?;
                    }
                }
                2 => {
                    let tmp = BlockStmtContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 2);
                    _localctx = tmp;
                    {
                        /*InvokeRule block*/
                        recog.base.set_state(122);
                        recog.block()?;
                    }
                }
                3 => {
                    let tmp = VarDeclContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 3);
                    _localctx = tmp;
                    {
                        /*InvokeRule decl*/
                        recog.base.set_state(123);
                        recog.decl()?;
                    }
                }
                4 => {
                    let tmp = AssContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 4);
                    _localctx = tmp;
                    {
                        /*InvokeRule lval*/
                        recog.base.set_state(124);
                        recog.lval_rec(0)?;

                        recog.base.set_state(125);
                        recog.base.match_token(T__8, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(126);
                        recog.expr_rec(0)?;

                        recog.base.set_state(127);
                        recog.base.match_token(T__7, &mut recog.err_handler)?;
                    }
                }
                5 => {
                    let tmp = IncrContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 5);
                    _localctx = tmp;
                    {
                        /*InvokeRule lval*/
                        recog.base.set_state(129);
                        recog.lval_rec(0)?;

                        recog.base.set_state(130);
                        recog.base.match_token(T__9, &mut recog.err_handler)?;

                        recog.base.set_state(131);
                        recog.base.match_token(T__7, &mut recog.err_handler)?;
                    }
                }
                6 => {
                    let tmp = DecrContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 6);
                    _localctx = tmp;
                    {
                        /*InvokeRule lval*/
                        recog.base.set_state(133);
                        recog.lval_rec(0)?;

                        recog.base.set_state(134);
                        recog.base.match_token(T__10, &mut recog.err_handler)?;

                        recog.base.set_state(135);
                        recog.base.match_token(T__7, &mut recog.err_handler)?;
                    }
                }
                7 => {
                    let tmp = RetContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 7);
                    _localctx = tmp;
                    {
                        recog.base.set_state(137);
                        recog.base.match_token(T__11, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(138);
                        recog.expr_rec(0)?;

                        recog.base.set_state(139);
                        recog.base.match_token(T__7, &mut recog.err_handler)?;
                    }
                }
                8 => {
                    let tmp = VRetContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 8);
                    _localctx = tmp;
                    {
                        recog.base.set_state(141);
                        recog.base.match_token(T__11, &mut recog.err_handler)?;

                        recog.base.set_state(142);
                        recog.base.match_token(T__7, &mut recog.err_handler)?;
                    }
                }
                9 => {
                    let tmp = CondContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 9);
                    _localctx = tmp;
                    {
                        recog.base.set_state(143);
                        recog.base.match_token(T__12, &mut recog.err_handler)?;

                        recog.base.set_state(144);
                        recog.base.match_token(T__2, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(145);
                        recog.expr_rec(0)?;

                        recog.base.set_state(146);
                        recog.base.match_token(T__3, &mut recog.err_handler)?;

                        /*InvokeRule stmt*/
                        recog.base.set_state(147);
                        recog.stmt()?;
                    }
                }
                10 => {
                    let tmp = CondElseContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 10);
                    _localctx = tmp;
                    {
                        recog.base.set_state(149);
                        recog.base.match_token(T__12, &mut recog.err_handler)?;

                        recog.base.set_state(150);
                        recog.base.match_token(T__2, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(151);
                        recog.expr_rec(0)?;

                        recog.base.set_state(152);
                        recog.base.match_token(T__3, &mut recog.err_handler)?;

                        /*InvokeRule stmt*/
                        recog.base.set_state(153);
                        recog.stmt()?;

                        recog.base.set_state(154);
                        recog.base.match_token(T__13, &mut recog.err_handler)?;

                        /*InvokeRule stmt*/
                        recog.base.set_state(155);
                        recog.stmt()?;
                    }
                }
                11 => {
                    let tmp = WhileContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 11);
                    _localctx = tmp;
                    {
                        recog.base.set_state(157);
                        recog.base.match_token(T__14, &mut recog.err_handler)?;

                        recog.base.set_state(158);
                        recog.base.match_token(T__2, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(159);
                        recog.expr_rec(0)?;

                        recog.base.set_state(160);
                        recog.base.match_token(T__3, &mut recog.err_handler)?;

                        /*InvokeRule stmt*/
                        recog.base.set_state(161);
                        recog.stmt()?;
                    }
                }
                12 => {
                    let tmp = SExpContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 12);
                    _localctx = tmp;
                    {
                        /*InvokeRule expr*/
                        recog.base.set_state(163);
                        recog.expr_rec(0)?;

                        recog.base.set_state(164);
                        recog.base.match_token(T__7, &mut recog.err_handler)?;
                    }
                }
                13 => {
                    let tmp = ForContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 13);
                    _localctx = tmp;
                    {
                        recog.base.set_state(166);
                        recog.base.match_token(T__15, &mut recog.err_handler)?;

                        recog.base.set_state(167);
                        recog.base.match_token(T__2, &mut recog.err_handler)?;

                        /*InvokeRule nonvoid_type*/
                        recog.base.set_state(168);
                        recog.nonvoid_type_rec(0)?;

                        recog.base.set_state(169);
                        recog.base.match_token(ID, &mut recog.err_handler)?;

                        recog.base.set_state(170);
                        recog.base.match_token(T__16, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(171);
                        recog.expr_rec(0)?;

                        recog.base.set_state(172);
                        recog.base.match_token(T__3, &mut recog.err_handler)?;

                        /*InvokeRule stmt*/
                        recog.base.set_state(173);
                        recog.stmt()?;
                    }
                }

                _ => {}
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- lval ----------------
#[derive(Debug)]
pub enum LvalContextAll<'input> {
    LFieldContext(LFieldContext<'input>),
    LIDContext(LIDContext<'input>),
    LArrContext(LArrContext<'input>),
    Error(LvalContext<'input>),
}
antlr_rust::tid! {LvalContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for LvalContextAll<'input> {}

impl<'input> LatteParserContext<'input> for LvalContextAll<'input> {}

impl<'input> Deref for LvalContextAll<'input> {
    type Target = dyn LvalContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use LvalContextAll::*;
        match self {
            LFieldContext(inner) => inner,
            LIDContext(inner) => inner,
            LArrContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for LvalContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for LvalContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type LvalContext<'input> = BaseParserRuleContext<'input, LvalContextExt<'input>>;

#[derive(Clone)]
pub struct LvalContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for LvalContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for LvalContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for LvalContext<'input> {}

impl<'input> CustomRuleContext<'input> for LvalContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_lval
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_lval }
}
antlr_rust::tid! {LvalContextExt<'a>}

impl<'input> LvalContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<LvalContextAll<'input>> {
        Rc::new(LvalContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                LvalContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait LvalContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<LvalContextExt<'input>>
{
}

impl<'input> LvalContextAttrs<'input> for LvalContext<'input> {}

pub type LFieldContext<'input> = BaseParserRuleContext<'input, LFieldContextExt<'input>>;

pub trait LFieldContextAttrs<'input>: LatteParserContext<'input> {
    fn lval(&self) -> Option<Rc<LvalContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> LFieldContextAttrs<'input> for LFieldContext<'input> {}

pub struct LFieldContextExt<'input> {
    base: LvalContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {LFieldContextExt<'a>}

impl<'input> LatteParserContext<'input> for LFieldContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for LFieldContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_LField(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_LField(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for LFieldContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_LField(self);
    }
}

impl<'input> CustomRuleContext<'input> for LFieldContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_lval
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_lval }
}

impl<'input> Borrow<LvalContextExt<'input>> for LFieldContext<'input> {
    fn borrow(&self) -> &LvalContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<LvalContextExt<'input>> for LFieldContext<'input> {
    fn borrow_mut(&mut self) -> &mut LvalContextExt<'input> {
        &mut self.base
    }
}

impl<'input> LvalContextAttrs<'input> for LFieldContext<'input> {}

impl<'input> LFieldContextExt<'input> {
    fn new(ctx: &dyn LvalContextAttrs<'input>) -> Rc<LvalContextAll<'input>> {
        Rc::new(LvalContextAll::LFieldContext(
            BaseParserRuleContext::copy_from(
                ctx,
                LFieldContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type LIDContext<'input> = BaseParserRuleContext<'input, LIDContextExt<'input>>;

pub trait LIDContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> LIDContextAttrs<'input> for LIDContext<'input> {}

pub struct LIDContextExt<'input> {
    base: LvalContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {LIDContextExt<'a>}

impl<'input> LatteParserContext<'input> for LIDContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for LIDContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_LID(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_LID(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for LIDContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_LID(self);
    }
}

impl<'input> CustomRuleContext<'input> for LIDContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_lval
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_lval }
}

impl<'input> Borrow<LvalContextExt<'input>> for LIDContext<'input> {
    fn borrow(&self) -> &LvalContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<LvalContextExt<'input>> for LIDContext<'input> {
    fn borrow_mut(&mut self) -> &mut LvalContextExt<'input> {
        &mut self.base
    }
}

impl<'input> LvalContextAttrs<'input> for LIDContext<'input> {}

impl<'input> LIDContextExt<'input> {
    fn new(ctx: &dyn LvalContextAttrs<'input>) -> Rc<LvalContextAll<'input>> {
        Rc::new(LvalContextAll::LIDContext(
            BaseParserRuleContext::copy_from(
                ctx,
                LIDContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type LArrContext<'input> = BaseParserRuleContext<'input, LArrContextExt<'input>>;

pub trait LArrContextAttrs<'input>: LatteParserContext<'input> {
    fn lval(&self) -> Option<Rc<LvalContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> LArrContextAttrs<'input> for LArrContext<'input> {}

pub struct LArrContextExt<'input> {
    base: LvalContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {LArrContextExt<'a>}

impl<'input> LatteParserContext<'input> for LArrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for LArrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_LArr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_LArr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for LArrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_LArr(self);
    }
}

impl<'input> CustomRuleContext<'input> for LArrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_lval
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_lval }
}

impl<'input> Borrow<LvalContextExt<'input>> for LArrContext<'input> {
    fn borrow(&self) -> &LvalContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<LvalContextExt<'input>> for LArrContext<'input> {
    fn borrow_mut(&mut self) -> &mut LvalContextExt<'input> {
        &mut self.base
    }
}

impl<'input> LvalContextAttrs<'input> for LArrContext<'input> {}

impl<'input> LArrContextExt<'input> {
    fn new(ctx: &dyn LvalContextAttrs<'input>) -> Rc<LvalContextAll<'input>> {
        Rc::new(LvalContextAll::LArrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                LArrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn lval(&mut self) -> Result<Rc<LvalContextAll<'input>>, ANTLRError> {
        self.lval_rec(0)
    }

    fn lval_rec(&mut self, _p: isize) -> Result<Rc<LvalContextAll<'input>>, ANTLRError> {
        let recog = self;
        let _parentctx = recog.ctx.take();
        let _parentState = recog.base.get_state();
        let mut _localctx = LvalContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog
            .base
            .enter_recursion_rule(_localctx.clone(), 24, RULE_lval, _p);
        let mut _localctx: Rc<LvalContextAll> = _localctx;
        let mut _prevctx = _localctx.clone();
        let _startState = 24;
        let result: Result<(), ANTLRError> = (|| {
            let mut _alt: isize;
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                {
                    let mut tmp = LIDContextExt::new(&**_localctx);
                    recog.ctx = Some(tmp.clone());
                    _localctx = tmp;
                    _prevctx = _localctx.clone();

                    recog.base.set_state(178);
                    recog.base.match_token(ID, &mut recog.err_handler)?;
                }

                let tmp = recog.input.lt(-1).cloned();
                recog.ctx.as_ref().unwrap().set_stop(tmp);
                recog.base.set_state(190);
                recog.err_handler.sync(&mut recog.base)?;
                _alt = recog.interpreter.adaptive_predict(11, &mut recog.base)?;
                while { _alt != 2 && _alt != INVALID_ALT } {
                    if _alt == 1 {
                        recog.trigger_exit_rule_event();
                        _prevctx = _localctx.clone();
                        {
                            recog.base.set_state(188);
                            recog.err_handler.sync(&mut recog.base)?;
                            match recog.interpreter.adaptive_predict(10, &mut recog.base)? {
                                1 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp =
                                            LFieldContextExt::new(&**LvalContextExt::new(
                                                _parentctx.clone(),
                                                _parentState,
                                            ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_lval,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(180);
                                        if !({ recog.precpred(None, 2) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 2)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        recog.base.set_state(181);
                                        recog.base.match_token(T__17, &mut recog.err_handler)?;

                                        recog.base.set_state(182);
                                        recog.base.match_token(ID, &mut recog.err_handler)?;
                                    }
                                }
                                2 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp = LArrContextExt::new(&**LvalContextExt::new(
                                            _parentctx.clone(),
                                            _parentState,
                                        ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_lval,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(183);
                                        if !({ recog.precpred(None, 1) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 1)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        recog.base.set_state(184);
                                        recog.base.match_token(T__18, &mut recog.err_handler)?;

                                        /*InvokeRule expr*/
                                        recog.base.set_state(185);
                                        recog.expr_rec(0)?;

                                        recog.base.set_state(186);
                                        recog.base.match_token(T__19, &mut recog.err_handler)?;
                                    }
                                }

                                _ => {}
                            }
                        }
                    }
                    recog.base.set_state(192);
                    recog.err_handler.sync(&mut recog.base)?;
                    _alt = recog.interpreter.adaptive_predict(11, &mut recog.base)?;
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.unroll_recursion_context(_parentctx);

        Ok(_localctx)
    }
}
//------------------- type_ ----------------
#[derive(Debug)]
pub enum Type_ContextAll<'input> {
    NonvoidContext(NonvoidContext<'input>),
    VoidContext(VoidContext<'input>),
    Error(Type_Context<'input>),
}
antlr_rust::tid! {Type_ContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for Type_ContextAll<'input> {}

impl<'input> LatteParserContext<'input> for Type_ContextAll<'input> {}

impl<'input> Deref for Type_ContextAll<'input> {
    type Target = dyn Type_ContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use Type_ContextAll::*;
        match self {
            NonvoidContext(inner) => inner,
            VoidContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for Type_ContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for Type_ContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type Type_Context<'input> = BaseParserRuleContext<'input, Type_ContextExt<'input>>;

#[derive(Clone)]
pub struct Type_ContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for Type_Context<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for Type_Context<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for Type_Context<'input> {}

impl<'input> CustomRuleContext<'input> for Type_ContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_type_
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_type_ }
}
antlr_rust::tid! {Type_ContextExt<'a>}

impl<'input> Type_ContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<Type_ContextAll<'input>> {
        Rc::new(Type_ContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                Type_ContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait Type_ContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<Type_ContextExt<'input>>
{
}

impl<'input> Type_ContextAttrs<'input> for Type_Context<'input> {}

pub type NonvoidContext<'input> = BaseParserRuleContext<'input, NonvoidContextExt<'input>>;

pub trait NonvoidContextAttrs<'input>: LatteParserContext<'input> {
    fn nonvoid_type(&self) -> Option<Rc<Nonvoid_typeContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> NonvoidContextAttrs<'input> for NonvoidContext<'input> {}

pub struct NonvoidContextExt<'input> {
    base: Type_ContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {NonvoidContextExt<'a>}

impl<'input> LatteParserContext<'input> for NonvoidContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NonvoidContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Nonvoid(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Nonvoid(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NonvoidContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Nonvoid(self);
    }
}

impl<'input> CustomRuleContext<'input> for NonvoidContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_type_
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_type_ }
}

impl<'input> Borrow<Type_ContextExt<'input>> for NonvoidContext<'input> {
    fn borrow(&self) -> &Type_ContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<Type_ContextExt<'input>> for NonvoidContext<'input> {
    fn borrow_mut(&mut self) -> &mut Type_ContextExt<'input> {
        &mut self.base
    }
}

impl<'input> Type_ContextAttrs<'input> for NonvoidContext<'input> {}

impl<'input> NonvoidContextExt<'input> {
    fn new(ctx: &dyn Type_ContextAttrs<'input>) -> Rc<Type_ContextAll<'input>> {
        Rc::new(Type_ContextAll::NonvoidContext(
            BaseParserRuleContext::copy_from(
                ctx,
                NonvoidContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type VoidContext<'input> = BaseParserRuleContext<'input, VoidContextExt<'input>>;

pub trait VoidContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> VoidContextAttrs<'input> for VoidContext<'input> {}

pub struct VoidContextExt<'input> {
    base: Type_ContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {VoidContextExt<'a>}

impl<'input> LatteParserContext<'input> for VoidContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for VoidContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Void(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Void(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for VoidContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Void(self);
    }
}

impl<'input> CustomRuleContext<'input> for VoidContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_type_
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_type_ }
}

impl<'input> Borrow<Type_ContextExt<'input>> for VoidContext<'input> {
    fn borrow(&self) -> &Type_ContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<Type_ContextExt<'input>> for VoidContext<'input> {
    fn borrow_mut(&mut self) -> &mut Type_ContextExt<'input> {
        &mut self.base
    }
}

impl<'input> Type_ContextAttrs<'input> for VoidContext<'input> {}

impl<'input> VoidContextExt<'input> {
    fn new(ctx: &dyn Type_ContextAttrs<'input>) -> Rc<Type_ContextAll<'input>> {
        Rc::new(Type_ContextAll::VoidContext(
            BaseParserRuleContext::copy_from(
                ctx,
                VoidContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn type_(&mut self) -> Result<Rc<Type_ContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = Type_ContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 26, RULE_type_);
        let mut _localctx: Rc<Type_ContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(195);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.base.input.la(1) {
                T__20 => {
                    let tmp = VoidContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 1);
                    _localctx = tmp;
                    {
                        recog.base.set_state(193);
                        recog.base.match_token(T__20, &mut recog.err_handler)?;
                    }
                }

                T__21 | T__22 | T__23 | ID => {
                    let tmp = NonvoidContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 2);
                    _localctx = tmp;
                    {
                        /*InvokeRule nonvoid_type*/
                        recog.base.set_state(194);
                        recog.nonvoid_type_rec(0)?;
                    }
                }

                _ => Err(ANTLRError::NoAltError(NoViableAltError::new(
                    &mut recog.base,
                )))?,
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- nonvoid_type ----------------
#[derive(Debug)]
pub enum Nonvoid_typeContextAll<'input> {
    StrContext(StrContext<'input>),
    ArrContext(ArrContext<'input>),
    BoolContext(BoolContext<'input>),
    ClassContext(ClassContext<'input>),
    IntContext(IntContext<'input>),
    Error(Nonvoid_typeContext<'input>),
}
antlr_rust::tid! {Nonvoid_typeContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for Nonvoid_typeContextAll<'input> {}

impl<'input> LatteParserContext<'input> for Nonvoid_typeContextAll<'input> {}

impl<'input> Deref for Nonvoid_typeContextAll<'input> {
    type Target = dyn Nonvoid_typeContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use Nonvoid_typeContextAll::*;
        match self {
            StrContext(inner) => inner,
            ArrContext(inner) => inner,
            BoolContext(inner) => inner,
            ClassContext(inner) => inner,
            IntContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for Nonvoid_typeContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for Nonvoid_typeContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type Nonvoid_typeContext<'input> =
    BaseParserRuleContext<'input, Nonvoid_typeContextExt<'input>>;

#[derive(Clone)]
pub struct Nonvoid_typeContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for Nonvoid_typeContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for Nonvoid_typeContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for Nonvoid_typeContext<'input> {}

impl<'input> CustomRuleContext<'input> for Nonvoid_typeContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_nonvoid_type
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_nonvoid_type }
}
antlr_rust::tid! {Nonvoid_typeContextExt<'a>}

impl<'input> Nonvoid_typeContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<Nonvoid_typeContextAll<'input>> {
        Rc::new(Nonvoid_typeContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                Nonvoid_typeContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait Nonvoid_typeContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<Nonvoid_typeContextExt<'input>>
{
}

impl<'input> Nonvoid_typeContextAttrs<'input> for Nonvoid_typeContext<'input> {}

pub type StrContext<'input> = BaseParserRuleContext<'input, StrContextExt<'input>>;

pub trait StrContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> StrContextAttrs<'input> for StrContext<'input> {}

pub struct StrContextExt<'input> {
    base: Nonvoid_typeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {StrContextExt<'a>}

impl<'input> LatteParserContext<'input> for StrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for StrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Str(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Str(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for StrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Str(self);
    }
}

impl<'input> CustomRuleContext<'input> for StrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_nonvoid_type
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_nonvoid_type }
}

impl<'input> Borrow<Nonvoid_typeContextExt<'input>> for StrContext<'input> {
    fn borrow(&self) -> &Nonvoid_typeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<Nonvoid_typeContextExt<'input>> for StrContext<'input> {
    fn borrow_mut(&mut self) -> &mut Nonvoid_typeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> Nonvoid_typeContextAttrs<'input> for StrContext<'input> {}

impl<'input> StrContextExt<'input> {
    fn new(ctx: &dyn Nonvoid_typeContextAttrs<'input>) -> Rc<Nonvoid_typeContextAll<'input>> {
        Rc::new(Nonvoid_typeContextAll::StrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                StrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type ArrContext<'input> = BaseParserRuleContext<'input, ArrContextExt<'input>>;

pub trait ArrContextAttrs<'input>: LatteParserContext<'input> {
    fn nonvoid_type(&self) -> Option<Rc<Nonvoid_typeContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> ArrContextAttrs<'input> for ArrContext<'input> {}

pub struct ArrContextExt<'input> {
    base: Nonvoid_typeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {ArrContextExt<'a>}

impl<'input> LatteParserContext<'input> for ArrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ArrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Arr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Arr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ArrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Arr(self);
    }
}

impl<'input> CustomRuleContext<'input> for ArrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_nonvoid_type
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_nonvoid_type }
}

impl<'input> Borrow<Nonvoid_typeContextExt<'input>> for ArrContext<'input> {
    fn borrow(&self) -> &Nonvoid_typeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<Nonvoid_typeContextExt<'input>> for ArrContext<'input> {
    fn borrow_mut(&mut self) -> &mut Nonvoid_typeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> Nonvoid_typeContextAttrs<'input> for ArrContext<'input> {}

impl<'input> ArrContextExt<'input> {
    fn new(ctx: &dyn Nonvoid_typeContextAttrs<'input>) -> Rc<Nonvoid_typeContextAll<'input>> {
        Rc::new(Nonvoid_typeContextAll::ArrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                ArrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type BoolContext<'input> = BaseParserRuleContext<'input, BoolContextExt<'input>>;

pub trait BoolContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> BoolContextAttrs<'input> for BoolContext<'input> {}

pub struct BoolContextExt<'input> {
    base: Nonvoid_typeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {BoolContextExt<'a>}

impl<'input> LatteParserContext<'input> for BoolContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for BoolContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Bool(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Bool(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for BoolContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Bool(self);
    }
}

impl<'input> CustomRuleContext<'input> for BoolContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_nonvoid_type
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_nonvoid_type }
}

impl<'input> Borrow<Nonvoid_typeContextExt<'input>> for BoolContext<'input> {
    fn borrow(&self) -> &Nonvoid_typeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<Nonvoid_typeContextExt<'input>> for BoolContext<'input> {
    fn borrow_mut(&mut self) -> &mut Nonvoid_typeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> Nonvoid_typeContextAttrs<'input> for BoolContext<'input> {}

impl<'input> BoolContextExt<'input> {
    fn new(ctx: &dyn Nonvoid_typeContextAttrs<'input>) -> Rc<Nonvoid_typeContextAll<'input>> {
        Rc::new(Nonvoid_typeContextAll::BoolContext(
            BaseParserRuleContext::copy_from(
                ctx,
                BoolContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type ClassContext<'input> = BaseParserRuleContext<'input, ClassContextExt<'input>>;

pub trait ClassContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> ClassContextAttrs<'input> for ClassContext<'input> {}

pub struct ClassContextExt<'input> {
    base: Nonvoid_typeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {ClassContextExt<'a>}

impl<'input> LatteParserContext<'input> for ClassContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ClassContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Class(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Class(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ClassContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Class(self);
    }
}

impl<'input> CustomRuleContext<'input> for ClassContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_nonvoid_type
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_nonvoid_type }
}

impl<'input> Borrow<Nonvoid_typeContextExt<'input>> for ClassContext<'input> {
    fn borrow(&self) -> &Nonvoid_typeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<Nonvoid_typeContextExt<'input>> for ClassContext<'input> {
    fn borrow_mut(&mut self) -> &mut Nonvoid_typeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> Nonvoid_typeContextAttrs<'input> for ClassContext<'input> {}

impl<'input> ClassContextExt<'input> {
    fn new(ctx: &dyn Nonvoid_typeContextAttrs<'input>) -> Rc<Nonvoid_typeContextAll<'input>> {
        Rc::new(Nonvoid_typeContextAll::ClassContext(
            BaseParserRuleContext::copy_from(
                ctx,
                ClassContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type IntContext<'input> = BaseParserRuleContext<'input, IntContextExt<'input>>;

pub trait IntContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> IntContextAttrs<'input> for IntContext<'input> {}

pub struct IntContextExt<'input> {
    base: Nonvoid_typeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {IntContextExt<'a>}

impl<'input> LatteParserContext<'input> for IntContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for IntContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_Int(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_Int(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for IntContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_Int(self);
    }
}

impl<'input> CustomRuleContext<'input> for IntContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_nonvoid_type
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_nonvoid_type }
}

impl<'input> Borrow<Nonvoid_typeContextExt<'input>> for IntContext<'input> {
    fn borrow(&self) -> &Nonvoid_typeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<Nonvoid_typeContextExt<'input>> for IntContext<'input> {
    fn borrow_mut(&mut self) -> &mut Nonvoid_typeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> Nonvoid_typeContextAttrs<'input> for IntContext<'input> {}

impl<'input> IntContextExt<'input> {
    fn new(ctx: &dyn Nonvoid_typeContextAttrs<'input>) -> Rc<Nonvoid_typeContextAll<'input>> {
        Rc::new(Nonvoid_typeContextAll::IntContext(
            BaseParserRuleContext::copy_from(
                ctx,
                IntContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn nonvoid_type(&mut self) -> Result<Rc<Nonvoid_typeContextAll<'input>>, ANTLRError> {
        self.nonvoid_type_rec(0)
    }

    fn nonvoid_type_rec(
        &mut self,
        _p: isize,
    ) -> Result<Rc<Nonvoid_typeContextAll<'input>>, ANTLRError> {
        let recog = self;
        let _parentctx = recog.ctx.take();
        let _parentState = recog.base.get_state();
        let mut _localctx = Nonvoid_typeContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog
            .base
            .enter_recursion_rule(_localctx.clone(), 28, RULE_nonvoid_type, _p);
        let mut _localctx: Rc<Nonvoid_typeContextAll> = _localctx;
        let mut _prevctx = _localctx.clone();
        let _startState = 28;
        let result: Result<(), ANTLRError> = (|| {
            let mut _alt: isize;
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(202);
                recog.err_handler.sync(&mut recog.base)?;
                match recog.base.input.la(1) {
                    T__21 => {
                        let mut tmp = IntContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();

                        recog.base.set_state(198);
                        recog.base.match_token(T__21, &mut recog.err_handler)?;
                    }

                    T__22 => {
                        let mut tmp = StrContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(199);
                        recog.base.match_token(T__22, &mut recog.err_handler)?;
                    }

                    T__23 => {
                        let mut tmp = BoolContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(200);
                        recog.base.match_token(T__23, &mut recog.err_handler)?;
                    }

                    ID => {
                        let mut tmp = ClassContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(201);
                        recog.base.match_token(ID, &mut recog.err_handler)?;
                    }

                    _ => Err(ANTLRError::NoAltError(NoViableAltError::new(
                        &mut recog.base,
                    )))?,
                }

                let tmp = recog.input.lt(-1).cloned();
                recog.ctx.as_ref().unwrap().set_stop(tmp);
                recog.base.set_state(208);
                recog.err_handler.sync(&mut recog.base)?;
                _alt = recog.interpreter.adaptive_predict(14, &mut recog.base)?;
                while { _alt != 2 && _alt != INVALID_ALT } {
                    if _alt == 1 {
                        recog.trigger_exit_rule_event();
                        _prevctx = _localctx.clone();
                        {
                            {
                                /*recRuleLabeledAltStartAction*/
                                let mut tmp = ArrContextExt::new(&**Nonvoid_typeContextExt::new(
                                    _parentctx.clone(),
                                    _parentState,
                                ));
                                recog.push_new_recursion_context(
                                    tmp.clone(),
                                    _startState,
                                    RULE_nonvoid_type,
                                );
                                _localctx = tmp;
                                recog.base.set_state(204);
                                if !({ recog.precpred(None, 2) }) {
                                    Err(FailedPredicateError::new(
                                        &mut recog.base,
                                        Some("recog.precpred(None, 2)".to_owned()),
                                        None,
                                    ))?;
                                }
                                recog.base.set_state(205);
                                recog.base.match_token(T__24, &mut recog.err_handler)?;
                            }
                        }
                    }
                    recog.base.set_state(210);
                    recog.err_handler.sync(&mut recog.base)?;
                    _alt = recog.interpreter.adaptive_predict(14, &mut recog.base)?;
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.unroll_recursion_context(_parentctx);

        Ok(_localctx)
    }
}
//------------------- newtype ----------------
#[derive(Debug)]
pub enum NewtypeContextAll<'input> {
    NBoolContext(NBoolContext<'input>),
    NIntContext(NIntContext<'input>),
    NClassContext(NClassContext<'input>),
    NStrContext(NStrContext<'input>),
    NArrContext(NArrContext<'input>),
    Error(NewtypeContext<'input>),
}
antlr_rust::tid! {NewtypeContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for NewtypeContextAll<'input> {}

impl<'input> LatteParserContext<'input> for NewtypeContextAll<'input> {}

impl<'input> Deref for NewtypeContextAll<'input> {
    type Target = dyn NewtypeContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use NewtypeContextAll::*;
        match self {
            NBoolContext(inner) => inner,
            NIntContext(inner) => inner,
            NClassContext(inner) => inner,
            NStrContext(inner) => inner,
            NArrContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NewtypeContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NewtypeContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type NewtypeContext<'input> = BaseParserRuleContext<'input, NewtypeContextExt<'input>>;

#[derive(Clone)]
pub struct NewtypeContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for NewtypeContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NewtypeContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NewtypeContext<'input> {}

impl<'input> CustomRuleContext<'input> for NewtypeContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_newtype
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_newtype }
}
antlr_rust::tid! {NewtypeContextExt<'a>}

impl<'input> NewtypeContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<NewtypeContextAll<'input>> {
        Rc::new(NewtypeContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                NewtypeContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait NewtypeContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<NewtypeContextExt<'input>>
{
}

impl<'input> NewtypeContextAttrs<'input> for NewtypeContext<'input> {}

pub type NBoolContext<'input> = BaseParserRuleContext<'input, NBoolContextExt<'input>>;

pub trait NBoolContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> NBoolContextAttrs<'input> for NBoolContext<'input> {}

pub struct NBoolContextExt<'input> {
    base: NewtypeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {NBoolContextExt<'a>}

impl<'input> LatteParserContext<'input> for NBoolContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NBoolContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_NBool(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_NBool(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NBoolContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_NBool(self);
    }
}

impl<'input> CustomRuleContext<'input> for NBoolContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_newtype
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_newtype }
}

impl<'input> Borrow<NewtypeContextExt<'input>> for NBoolContext<'input> {
    fn borrow(&self) -> &NewtypeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<NewtypeContextExt<'input>> for NBoolContext<'input> {
    fn borrow_mut(&mut self) -> &mut NewtypeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> NewtypeContextAttrs<'input> for NBoolContext<'input> {}

impl<'input> NBoolContextExt<'input> {
    fn new(ctx: &dyn NewtypeContextAttrs<'input>) -> Rc<NewtypeContextAll<'input>> {
        Rc::new(NewtypeContextAll::NBoolContext(
            BaseParserRuleContext::copy_from(
                ctx,
                NBoolContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type NIntContext<'input> = BaseParserRuleContext<'input, NIntContextExt<'input>>;

pub trait NIntContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> NIntContextAttrs<'input> for NIntContext<'input> {}

pub struct NIntContextExt<'input> {
    base: NewtypeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {NIntContextExt<'a>}

impl<'input> LatteParserContext<'input> for NIntContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NIntContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_NInt(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_NInt(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NIntContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_NInt(self);
    }
}

impl<'input> CustomRuleContext<'input> for NIntContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_newtype
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_newtype }
}

impl<'input> Borrow<NewtypeContextExt<'input>> for NIntContext<'input> {
    fn borrow(&self) -> &NewtypeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<NewtypeContextExt<'input>> for NIntContext<'input> {
    fn borrow_mut(&mut self) -> &mut NewtypeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> NewtypeContextAttrs<'input> for NIntContext<'input> {}

impl<'input> NIntContextExt<'input> {
    fn new(ctx: &dyn NewtypeContextAttrs<'input>) -> Rc<NewtypeContextAll<'input>> {
        Rc::new(NewtypeContextAll::NIntContext(
            BaseParserRuleContext::copy_from(
                ctx,
                NIntContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type NClassContext<'input> = BaseParserRuleContext<'input, NClassContextExt<'input>>;

pub trait NClassContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> NClassContextAttrs<'input> for NClassContext<'input> {}

pub struct NClassContextExt<'input> {
    base: NewtypeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {NClassContextExt<'a>}

impl<'input> LatteParserContext<'input> for NClassContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NClassContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_NClass(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_NClass(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NClassContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_NClass(self);
    }
}

impl<'input> CustomRuleContext<'input> for NClassContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_newtype
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_newtype }
}

impl<'input> Borrow<NewtypeContextExt<'input>> for NClassContext<'input> {
    fn borrow(&self) -> &NewtypeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<NewtypeContextExt<'input>> for NClassContext<'input> {
    fn borrow_mut(&mut self) -> &mut NewtypeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> NewtypeContextAttrs<'input> for NClassContext<'input> {}

impl<'input> NClassContextExt<'input> {
    fn new(ctx: &dyn NewtypeContextAttrs<'input>) -> Rc<NewtypeContextAll<'input>> {
        Rc::new(NewtypeContextAll::NClassContext(
            BaseParserRuleContext::copy_from(
                ctx,
                NClassContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type NStrContext<'input> = BaseParserRuleContext<'input, NStrContextExt<'input>>;

pub trait NStrContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> NStrContextAttrs<'input> for NStrContext<'input> {}

pub struct NStrContextExt<'input> {
    base: NewtypeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {NStrContextExt<'a>}

impl<'input> LatteParserContext<'input> for NStrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NStrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_NStr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_NStr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NStrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_NStr(self);
    }
}

impl<'input> CustomRuleContext<'input> for NStrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_newtype
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_newtype }
}

impl<'input> Borrow<NewtypeContextExt<'input>> for NStrContext<'input> {
    fn borrow(&self) -> &NewtypeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<NewtypeContextExt<'input>> for NStrContext<'input> {
    fn borrow_mut(&mut self) -> &mut NewtypeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> NewtypeContextAttrs<'input> for NStrContext<'input> {}

impl<'input> NStrContextExt<'input> {
    fn new(ctx: &dyn NewtypeContextAttrs<'input>) -> Rc<NewtypeContextAll<'input>> {
        Rc::new(NewtypeContextAll::NStrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                NStrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type NArrContext<'input> = BaseParserRuleContext<'input, NArrContextExt<'input>>;

pub trait NArrContextAttrs<'input>: LatteParserContext<'input> {
    fn type_(&self) -> Option<Rc<Type_ContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> NArrContextAttrs<'input> for NArrContext<'input> {}

pub struct NArrContextExt<'input> {
    base: NewtypeContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {NArrContextExt<'a>}

impl<'input> LatteParserContext<'input> for NArrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for NArrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_NArr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_NArr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for NArrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_NArr(self);
    }
}

impl<'input> CustomRuleContext<'input> for NArrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_newtype
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_newtype }
}

impl<'input> Borrow<NewtypeContextExt<'input>> for NArrContext<'input> {
    fn borrow(&self) -> &NewtypeContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<NewtypeContextExt<'input>> for NArrContext<'input> {
    fn borrow_mut(&mut self) -> &mut NewtypeContextExt<'input> {
        &mut self.base
    }
}

impl<'input> NewtypeContextAttrs<'input> for NArrContext<'input> {}

impl<'input> NArrContextExt<'input> {
    fn new(ctx: &dyn NewtypeContextAttrs<'input>) -> Rc<NewtypeContextAll<'input>> {
        Rc::new(NewtypeContextAll::NArrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                NArrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn newtype(&mut self) -> Result<Rc<NewtypeContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = NewtypeContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 30, RULE_newtype);
        let mut _localctx: Rc<NewtypeContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(220);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.interpreter.adaptive_predict(15, &mut recog.base)? {
                1 => {
                    let tmp = NIntContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 1);
                    _localctx = tmp;
                    {
                        recog.base.set_state(211);
                        recog.base.match_token(T__21, &mut recog.err_handler)?;
                    }
                }
                2 => {
                    let tmp = NStrContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 2);
                    _localctx = tmp;
                    {
                        recog.base.set_state(212);
                        recog.base.match_token(T__22, &mut recog.err_handler)?;
                    }
                }
                3 => {
                    let tmp = NBoolContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 3);
                    _localctx = tmp;
                    {
                        recog.base.set_state(213);
                        recog.base.match_token(T__23, &mut recog.err_handler)?;
                    }
                }
                4 => {
                    let tmp = NArrContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 4);
                    _localctx = tmp;
                    {
                        /*InvokeRule type_*/
                        recog.base.set_state(214);
                        recog.type_()?;

                        recog.base.set_state(215);
                        recog.base.match_token(T__18, &mut recog.err_handler)?;

                        /*InvokeRule expr*/
                        recog.base.set_state(216);
                        recog.expr_rec(0)?;

                        recog.base.set_state(217);
                        recog.base.match_token(T__19, &mut recog.err_handler)?;
                    }
                }
                5 => {
                    let tmp = NClassContextExt::new(&**_localctx);
                    recog.base.enter_outer_alt(Some(tmp.clone()), 5);
                    _localctx = tmp;
                    {
                        recog.base.set_state(219);
                        recog.base.match_token(ID, &mut recog.err_handler)?;
                    }
                }

                _ => {}
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- expr ----------------
#[derive(Debug)]
pub enum ExprContextAll<'input> {
    EIdContext(EIdContext<'input>),
    EFunCallContext(EFunCallContext<'input>),
    ERelOpContext(ERelOpContext<'input>),
    ETrueContext(ETrueContext<'input>),
    EOrContext(EOrContext<'input>),
    EIntContext(EIntContext<'input>),
    EUnOpContext(EUnOpContext<'input>),
    EStrContext(EStrContext<'input>),
    EArrSubContext(EArrSubContext<'input>),
    EMulOpContext(EMulOpContext<'input>),
    EAndContext(EAndContext<'input>),
    EParenContext(EParenContext<'input>),
    EFalseContext(EFalseContext<'input>),
    EMetCallContext(EMetCallContext<'input>),
    ENewContext(ENewContext<'input>),
    EAddOpContext(EAddOpContext<'input>),
    ENullContext(ENullContext<'input>),
    EFieldContext(EFieldContext<'input>),
    Error(ExprContext<'input>),
}
antlr_rust::tid! {ExprContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for ExprContextAll<'input> {}

impl<'input> LatteParserContext<'input> for ExprContextAll<'input> {}

impl<'input> Deref for ExprContextAll<'input> {
    type Target = dyn ExprContextAttrs<'input> + 'input;
    fn deref(&self) -> &Self::Target {
        use ExprContextAll::*;
        match self {
            EIdContext(inner) => inner,
            EFunCallContext(inner) => inner,
            ERelOpContext(inner) => inner,
            ETrueContext(inner) => inner,
            EOrContext(inner) => inner,
            EIntContext(inner) => inner,
            EUnOpContext(inner) => inner,
            EStrContext(inner) => inner,
            EArrSubContext(inner) => inner,
            EMulOpContext(inner) => inner,
            EAndContext(inner) => inner,
            EParenContext(inner) => inner,
            EFalseContext(inner) => inner,
            EMetCallContext(inner) => inner,
            ENewContext(inner) => inner,
            EAddOpContext(inner) => inner,
            ENullContext(inner) => inner,
            EFieldContext(inner) => inner,
            Error(inner) => inner,
        }
    }
}
impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ExprContextAll<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        self.deref().accept(visitor)
    }
}
impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ExprContextAll<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().enter(listener)
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        self.deref().exit(listener)
    }
}

pub type ExprContext<'input> = BaseParserRuleContext<'input, ExprContextExt<'input>>;

#[derive(Clone)]
pub struct ExprContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ExprContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ExprContext<'input> {}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ExprContext<'input> {}

impl<'input> CustomRuleContext<'input> for ExprContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}
antlr_rust::tid! {ExprContextExt<'a>}

impl<'input> ExprContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::Error(
            BaseParserRuleContext::new_parser_ctx(
                parent,
                invoking_state,
                ExprContextExt { ph: PhantomData },
            ),
        ))
    }
}

pub trait ExprContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ExprContextExt<'input>>
{
}

impl<'input> ExprContextAttrs<'input> for ExprContext<'input> {}

pub type EIdContext<'input> = BaseParserRuleContext<'input, EIdContextExt<'input>>;

pub trait EIdContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> EIdContextAttrs<'input> for EIdContext<'input> {}

pub struct EIdContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EIdContextExt<'a>}

impl<'input> LatteParserContext<'input> for EIdContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EIdContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EId(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EId(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EIdContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EId(self);
    }
}

impl<'input> CustomRuleContext<'input> for EIdContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EIdContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EIdContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EIdContext<'input> {}

impl<'input> EIdContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EIdContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EIdContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EFunCallContext<'input> = BaseParserRuleContext<'input, EFunCallContextExt<'input>>;

pub trait EFunCallContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
    fn args(&self) -> Option<Rc<ArgsContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> EFunCallContextAttrs<'input> for EFunCallContext<'input> {}

pub struct EFunCallContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EFunCallContextExt<'a>}

impl<'input> LatteParserContext<'input> for EFunCallContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EFunCallContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EFunCall(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EFunCall(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EFunCallContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EFunCall(self);
    }
}

impl<'input> CustomRuleContext<'input> for EFunCallContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EFunCallContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EFunCallContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EFunCallContext<'input> {}

impl<'input> EFunCallContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EFunCallContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EFunCallContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type ERelOpContext<'input> = BaseParserRuleContext<'input, ERelOpContextExt<'input>>;

pub trait ERelOpContextAttrs<'input>: LatteParserContext<'input> {
    fn expr_all(&self) -> Vec<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
    fn relOp(&self) -> Option<Rc<RelOpContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> ERelOpContextAttrs<'input> for ERelOpContext<'input> {}

pub struct ERelOpContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {ERelOpContextExt<'a>}

impl<'input> LatteParserContext<'input> for ERelOpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ERelOpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_ERelOp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_ERelOp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ERelOpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_ERelOp(self);
    }
}

impl<'input> CustomRuleContext<'input> for ERelOpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ERelOpContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ERelOpContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for ERelOpContext<'input> {}

impl<'input> ERelOpContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::ERelOpContext(
            BaseParserRuleContext::copy_from(
                ctx,
                ERelOpContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type ETrueContext<'input> = BaseParserRuleContext<'input, ETrueContextExt<'input>>;

pub trait ETrueContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> ETrueContextAttrs<'input> for ETrueContext<'input> {}

pub struct ETrueContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {ETrueContextExt<'a>}

impl<'input> LatteParserContext<'input> for ETrueContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ETrueContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_ETrue(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_ETrue(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ETrueContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_ETrue(self);
    }
}

impl<'input> CustomRuleContext<'input> for ETrueContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ETrueContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ETrueContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for ETrueContext<'input> {}

impl<'input> ETrueContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::ETrueContext(
            BaseParserRuleContext::copy_from(
                ctx,
                ETrueContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EOrContext<'input> = BaseParserRuleContext<'input, EOrContextExt<'input>>;

pub trait EOrContextAttrs<'input>: LatteParserContext<'input> {
    fn expr_all(&self) -> Vec<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> EOrContextAttrs<'input> for EOrContext<'input> {}

pub struct EOrContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EOrContextExt<'a>}

impl<'input> LatteParserContext<'input> for EOrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EOrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EOr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EOr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EOrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EOr(self);
    }
}

impl<'input> CustomRuleContext<'input> for EOrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EOrContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EOrContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EOrContext<'input> {}

impl<'input> EOrContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EOrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EOrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EIntContext<'input> = BaseParserRuleContext<'input, EIntContextExt<'input>>;

pub trait EIntContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token INT
    /// Returns `None` if there is no child corresponding to token INT
    fn INT(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(INT, 0)
    }
}

impl<'input> EIntContextAttrs<'input> for EIntContext<'input> {}

pub struct EIntContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EIntContextExt<'a>}

impl<'input> LatteParserContext<'input> for EIntContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EIntContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EInt(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EInt(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EIntContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EInt(self);
    }
}

impl<'input> CustomRuleContext<'input> for EIntContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EIntContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EIntContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EIntContext<'input> {}

impl<'input> EIntContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EIntContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EIntContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EUnOpContext<'input> = BaseParserRuleContext<'input, EUnOpContextExt<'input>>;

pub trait EUnOpContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> EUnOpContextAttrs<'input> for EUnOpContext<'input> {}

pub struct EUnOpContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EUnOpContextExt<'a>}

impl<'input> LatteParserContext<'input> for EUnOpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EUnOpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EUnOp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EUnOp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EUnOpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EUnOp(self);
    }
}

impl<'input> CustomRuleContext<'input> for EUnOpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EUnOpContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EUnOpContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EUnOpContext<'input> {}

impl<'input> EUnOpContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EUnOpContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EUnOpContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EStrContext<'input> = BaseParserRuleContext<'input, EStrContextExt<'input>>;

pub trait EStrContextAttrs<'input>: LatteParserContext<'input> {
    /// Retrieves first TerminalNode corresponding to token STR
    /// Returns `None` if there is no child corresponding to token STR
    fn STR(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(STR, 0)
    }
}

impl<'input> EStrContextAttrs<'input> for EStrContext<'input> {}

pub struct EStrContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EStrContextExt<'a>}

impl<'input> LatteParserContext<'input> for EStrContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EStrContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EStr(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EStr(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EStrContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EStr(self);
    }
}

impl<'input> CustomRuleContext<'input> for EStrContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EStrContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EStrContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EStrContext<'input> {}

impl<'input> EStrContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EStrContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EStrContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EArrSubContext<'input> = BaseParserRuleContext<'input, EArrSubContextExt<'input>>;

pub trait EArrSubContextAttrs<'input>: LatteParserContext<'input> {
    fn expr_all(&self) -> Vec<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> EArrSubContextAttrs<'input> for EArrSubContext<'input> {}

pub struct EArrSubContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EArrSubContextExt<'a>}

impl<'input> LatteParserContext<'input> for EArrSubContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EArrSubContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EArrSub(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EArrSub(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EArrSubContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EArrSub(self);
    }
}

impl<'input> CustomRuleContext<'input> for EArrSubContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EArrSubContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EArrSubContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EArrSubContext<'input> {}

impl<'input> EArrSubContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EArrSubContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EArrSubContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EMulOpContext<'input> = BaseParserRuleContext<'input, EMulOpContextExt<'input>>;

pub trait EMulOpContextAttrs<'input>: LatteParserContext<'input> {
    fn expr_all(&self) -> Vec<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
    fn mulOp(&self) -> Option<Rc<MulOpContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> EMulOpContextAttrs<'input> for EMulOpContext<'input> {}

pub struct EMulOpContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EMulOpContextExt<'a>}

impl<'input> LatteParserContext<'input> for EMulOpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EMulOpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EMulOp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EMulOp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EMulOpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EMulOp(self);
    }
}

impl<'input> CustomRuleContext<'input> for EMulOpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EMulOpContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EMulOpContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EMulOpContext<'input> {}

impl<'input> EMulOpContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EMulOpContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EMulOpContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EAndContext<'input> = BaseParserRuleContext<'input, EAndContextExt<'input>>;

pub trait EAndContextAttrs<'input>: LatteParserContext<'input> {
    fn expr_all(&self) -> Vec<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> EAndContextAttrs<'input> for EAndContext<'input> {}

pub struct EAndContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EAndContextExt<'a>}

impl<'input> LatteParserContext<'input> for EAndContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EAndContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EAnd(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EAnd(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EAndContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EAnd(self);
    }
}

impl<'input> CustomRuleContext<'input> for EAndContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EAndContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EAndContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EAndContext<'input> {}

impl<'input> EAndContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EAndContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EAndContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EParenContext<'input> = BaseParserRuleContext<'input, EParenContextExt<'input>>;

pub trait EParenContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> EParenContextAttrs<'input> for EParenContext<'input> {}

pub struct EParenContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EParenContextExt<'a>}

impl<'input> LatteParserContext<'input> for EParenContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EParenContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EParen(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EParen(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EParenContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EParen(self);
    }
}

impl<'input> CustomRuleContext<'input> for EParenContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EParenContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EParenContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EParenContext<'input> {}

impl<'input> EParenContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EParenContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EParenContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EFalseContext<'input> = BaseParserRuleContext<'input, EFalseContextExt<'input>>;

pub trait EFalseContextAttrs<'input>: LatteParserContext<'input> {}

impl<'input> EFalseContextAttrs<'input> for EFalseContext<'input> {}

pub struct EFalseContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EFalseContextExt<'a>}

impl<'input> LatteParserContext<'input> for EFalseContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EFalseContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EFalse(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EFalse(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EFalseContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EFalse(self);
    }
}

impl<'input> CustomRuleContext<'input> for EFalseContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EFalseContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EFalseContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EFalseContext<'input> {}

impl<'input> EFalseContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EFalseContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EFalseContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EMetCallContext<'input> = BaseParserRuleContext<'input, EMetCallContextExt<'input>>;

pub trait EMetCallContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
    fn args(&self) -> Option<Rc<ArgsContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> EMetCallContextAttrs<'input> for EMetCallContext<'input> {}

pub struct EMetCallContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EMetCallContextExt<'a>}

impl<'input> LatteParserContext<'input> for EMetCallContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EMetCallContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EMetCall(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EMetCall(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EMetCallContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EMetCall(self);
    }
}

impl<'input> CustomRuleContext<'input> for EMetCallContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EMetCallContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EMetCallContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EMetCallContext<'input> {}

impl<'input> EMetCallContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EMetCallContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EMetCallContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type ENewContext<'input> = BaseParserRuleContext<'input, ENewContextExt<'input>>;

pub trait ENewContextAttrs<'input>: LatteParserContext<'input> {
    fn newtype(&self) -> Option<Rc<NewtypeContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> ENewContextAttrs<'input> for ENewContext<'input> {}

pub struct ENewContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {ENewContextExt<'a>}

impl<'input> LatteParserContext<'input> for ENewContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ENewContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_ENew(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_ENew(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ENewContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_ENew(self);
    }
}

impl<'input> CustomRuleContext<'input> for ENewContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ENewContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ENewContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for ENewContext<'input> {}

impl<'input> ENewContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::ENewContext(
            BaseParserRuleContext::copy_from(
                ctx,
                ENewContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EAddOpContext<'input> = BaseParserRuleContext<'input, EAddOpContextExt<'input>>;

pub trait EAddOpContextAttrs<'input>: LatteParserContext<'input> {
    fn expr_all(&self) -> Vec<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
    fn addOp(&self) -> Option<Rc<AddOpContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> EAddOpContextAttrs<'input> for EAddOpContext<'input> {}

pub struct EAddOpContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EAddOpContextExt<'a>}

impl<'input> LatteParserContext<'input> for EAddOpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EAddOpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EAddOp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EAddOp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EAddOpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EAddOp(self);
    }
}

impl<'input> CustomRuleContext<'input> for EAddOpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EAddOpContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EAddOpContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EAddOpContext<'input> {}

impl<'input> EAddOpContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EAddOpContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EAddOpContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type ENullContext<'input> = BaseParserRuleContext<'input, ENullContextExt<'input>>;

pub trait ENullContextAttrs<'input>: LatteParserContext<'input> {
    fn nonvoid_type(&self) -> Option<Rc<Nonvoid_typeContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> ENullContextAttrs<'input> for ENullContext<'input> {}

pub struct ENullContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {ENullContextExt<'a>}

impl<'input> LatteParserContext<'input> for ENullContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ENullContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_ENull(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_ENull(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ENullContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_ENull(self);
    }
}

impl<'input> CustomRuleContext<'input> for ENullContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ENullContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ENullContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for ENullContext<'input> {}

impl<'input> ENullContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::ENullContext(
            BaseParserRuleContext::copy_from(
                ctx,
                ENullContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

pub type EFieldContext<'input> = BaseParserRuleContext<'input, EFieldContextExt<'input>>;

pub trait EFieldContextAttrs<'input>: LatteParserContext<'input> {
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
    /// Retrieves first TerminalNode corresponding to token ID
    /// Returns `None` if there is no child corresponding to token ID
    fn ID(&self) -> Option<Rc<TerminalNode<'input, LatteParserContextType>>>
    where
        Self: Sized,
    {
        self.get_token(ID, 0)
    }
}

impl<'input> EFieldContextAttrs<'input> for EFieldContext<'input> {}

pub struct EFieldContextExt<'input> {
    base: ExprContextExt<'input>,
    ph: PhantomData<&'input str>,
}

antlr_rust::tid! {EFieldContextExt<'a>}

impl<'input> LatteParserContext<'input> for EFieldContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for EFieldContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_EField(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_EField(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for EFieldContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_EField(self);
    }
}

impl<'input> CustomRuleContext<'input> for EFieldContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_expr
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EFieldContext<'input> {
    fn borrow(&self) -> &ExprContextExt<'input> {
        &self.base
    }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EFieldContext<'input> {
    fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> {
        &mut self.base
    }
}

impl<'input> ExprContextAttrs<'input> for EFieldContext<'input> {}

impl<'input> EFieldContextExt<'input> {
    fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>> {
        Rc::new(ExprContextAll::EFieldContext(
            BaseParserRuleContext::copy_from(
                ctx,
                EFieldContextExt {
                    base: ctx.borrow().clone(),
                    ph: PhantomData,
                },
            ),
        ))
    }
}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn expr(&mut self) -> Result<Rc<ExprContextAll<'input>>, ANTLRError> {
        self.expr_rec(0)
    }

    fn expr_rec(&mut self, _p: isize) -> Result<Rc<ExprContextAll<'input>>, ANTLRError> {
        let recog = self;
        let _parentctx = recog.ctx.take();
        let _parentState = recog.base.get_state();
        let mut _localctx = ExprContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog
            .base
            .enter_recursion_rule(_localctx.clone(), 32, RULE_expr, _p);
        let mut _localctx: Rc<ExprContextAll> = _localctx;
        let mut _prevctx = _localctx.clone();
        let _startState = 32;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            let mut _alt: isize;
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(246);
                recog.err_handler.sync(&mut recog.base)?;
                match recog.interpreter.adaptive_predict(16, &mut recog.base)? {
                    1 => {
                        {
                            let mut tmp = EUnOpContextExt::new(&**_localctx);
                            recog.ctx = Some(tmp.clone());
                            _localctx = tmp;
                            _prevctx = _localctx.clone();

                            recog.base.set_state(223);
                            _la = recog.base.input.la(1);
                            if { !(_la == T__25 || _la == T__26) } {
                                recog.err_handler.recover_inline(&mut recog.base)?;
                            } else {
                                if recog.base.input.la(1) == TOKEN_EOF {
                                    recog.base.matched_eof = true
                                };
                                recog.err_handler.report_match(&mut recog.base);
                                recog.base.consume(&mut recog.err_handler);
                            }
                            /*InvokeRule expr*/
                            recog.base.set_state(224);
                            recog.expr_rec(18)?;
                        }
                    }
                    2 => {
                        let mut tmp = EIdContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(225);
                        recog.base.match_token(ID, &mut recog.err_handler)?;
                    }
                    3 => {
                        let mut tmp = EIntContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(226);
                        recog.base.match_token(INT, &mut recog.err_handler)?;
                    }
                    4 => {
                        let mut tmp = ETrueContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(227);
                        recog.base.match_token(T__29, &mut recog.err_handler)?;
                    }
                    5 => {
                        let mut tmp = EFalseContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(228);
                        recog.base.match_token(T__30, &mut recog.err_handler)?;
                    }
                    6 => {
                        {
                            let mut tmp = EFunCallContextExt::new(&**_localctx);
                            recog.ctx = Some(tmp.clone());
                            _localctx = tmp;
                            _prevctx = _localctx.clone();
                            recog.base.set_state(229);
                            recog.base.match_token(ID, &mut recog.err_handler)?;

                            recog.base.set_state(230);
                            recog.base.match_token(T__2, &mut recog.err_handler)?;

                            /*InvokeRule args*/
                            recog.base.set_state(231);
                            recog.args()?;

                            recog.base.set_state(232);
                            recog.base.match_token(T__3, &mut recog.err_handler)?;
                        }
                    }
                    7 => {
                        let mut tmp = EStrContextExt::new(&**_localctx);
                        recog.ctx = Some(tmp.clone());
                        _localctx = tmp;
                        _prevctx = _localctx.clone();
                        recog.base.set_state(234);
                        recog.base.match_token(STR, &mut recog.err_handler)?;
                    }
                    8 => {
                        {
                            let mut tmp = ENullContextExt::new(&**_localctx);
                            recog.ctx = Some(tmp.clone());
                            _localctx = tmp;
                            _prevctx = _localctx.clone();
                            recog.base.set_state(235);
                            recog.base.match_token(T__2, &mut recog.err_handler)?;

                            /*InvokeRule nonvoid_type*/
                            recog.base.set_state(236);
                            recog.nonvoid_type_rec(0)?;

                            recog.base.set_state(237);
                            recog.base.match_token(T__3, &mut recog.err_handler)?;

                            recog.base.set_state(238);
                            recog.base.match_token(T__31, &mut recog.err_handler)?;
                        }
                    }
                    9 => {
                        {
                            let mut tmp = EParenContextExt::new(&**_localctx);
                            recog.ctx = Some(tmp.clone());
                            _localctx = tmp;
                            _prevctx = _localctx.clone();
                            recog.base.set_state(240);
                            recog.base.match_token(T__2, &mut recog.err_handler)?;

                            /*InvokeRule expr*/
                            recog.base.set_state(241);
                            recog.expr_rec(0)?;

                            recog.base.set_state(242);
                            recog.base.match_token(T__3, &mut recog.err_handler)?;
                        }
                    }
                    10 => {
                        {
                            let mut tmp = ENewContextExt::new(&**_localctx);
                            recog.ctx = Some(tmp.clone());
                            _localctx = tmp;
                            _prevctx = _localctx.clone();
                            recog.base.set_state(244);
                            recog.base.match_token(T__32, &mut recog.err_handler)?;

                            /*InvokeRule newtype*/
                            recog.base.set_state(245);
                            recog.newtype()?;
                        }
                    }

                    _ => {}
                }

                let tmp = recog.input.lt(-1).cloned();
                recog.ctx.as_ref().unwrap().set_stop(tmp);
                recog.base.set_state(283);
                recog.err_handler.sync(&mut recog.base)?;
                _alt = recog.interpreter.adaptive_predict(18, &mut recog.base)?;
                while { _alt != 2 && _alt != INVALID_ALT } {
                    if _alt == 1 {
                        recog.trigger_exit_rule_event();
                        _prevctx = _localctx.clone();
                        {
                            recog.base.set_state(281);
                            recog.err_handler.sync(&mut recog.base)?;
                            match recog.interpreter.adaptive_predict(17, &mut recog.base)? {
                                1 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp =
                                            EMulOpContextExt::new(&**ExprContextExt::new(
                                                _parentctx.clone(),
                                                _parentState,
                                            ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(248);
                                        if !({ recog.precpred(None, 17) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 17)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        /*InvokeRule mulOp*/
                                        recog.base.set_state(249);
                                        recog.mulOp()?;

                                        /*InvokeRule expr*/
                                        recog.base.set_state(250);
                                        recog.expr_rec(18)?;
                                    }
                                }
                                2 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp =
                                            EAddOpContextExt::new(&**ExprContextExt::new(
                                                _parentctx.clone(),
                                                _parentState,
                                            ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(252);
                                        if !({ recog.precpred(None, 16) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 16)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        /*InvokeRule addOp*/
                                        recog.base.set_state(253);
                                        recog.addOp()?;

                                        /*InvokeRule expr*/
                                        recog.base.set_state(254);
                                        recog.expr_rec(17)?;
                                    }
                                }
                                3 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp =
                                            ERelOpContextExt::new(&**ExprContextExt::new(
                                                _parentctx.clone(),
                                                _parentState,
                                            ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(256);
                                        if !({ recog.precpred(None, 15) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 15)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        /*InvokeRule relOp*/
                                        recog.base.set_state(257);
                                        recog.relOp()?;

                                        /*InvokeRule expr*/
                                        recog.base.set_state(258);
                                        recog.expr_rec(16)?;
                                    }
                                }
                                4 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp = EAndContextExt::new(&**ExprContextExt::new(
                                            _parentctx.clone(),
                                            _parentState,
                                        ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(260);
                                        if !({ recog.precpred(None, 14) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 14)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        recog.base.set_state(261);
                                        recog.base.match_token(T__27, &mut recog.err_handler)?;

                                        /*InvokeRule expr*/
                                        recog.base.set_state(262);
                                        recog.expr_rec(14)?;
                                    }
                                }
                                5 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp = EOrContextExt::new(&**ExprContextExt::new(
                                            _parentctx.clone(),
                                            _parentState,
                                        ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(263);
                                        if !({ recog.precpred(None, 13) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 13)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        recog.base.set_state(264);
                                        recog.base.match_token(T__28, &mut recog.err_handler)?;

                                        /*InvokeRule expr*/
                                        recog.base.set_state(265);
                                        recog.expr_rec(13)?;
                                    }
                                }
                                6 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp =
                                            EArrSubContextExt::new(&**ExprContextExt::new(
                                                _parentctx.clone(),
                                                _parentState,
                                            ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(266);
                                        if !({ recog.precpred(None, 4) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 4)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        recog.base.set_state(267);
                                        recog.base.match_token(T__18, &mut recog.err_handler)?;

                                        /*InvokeRule expr*/
                                        recog.base.set_state(268);
                                        recog.expr_rec(0)?;

                                        recog.base.set_state(269);
                                        recog.base.match_token(T__19, &mut recog.err_handler)?;
                                    }
                                }
                                7 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp =
                                            EFieldContextExt::new(&**ExprContextExt::new(
                                                _parentctx.clone(),
                                                _parentState,
                                            ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(271);
                                        if !({ recog.precpred(None, 3) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 3)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        recog.base.set_state(272);
                                        recog.base.match_token(T__17, &mut recog.err_handler)?;

                                        recog.base.set_state(273);
                                        recog.base.match_token(ID, &mut recog.err_handler)?;
                                    }
                                }
                                8 => {
                                    {
                                        /*recRuleLabeledAltStartAction*/
                                        let mut tmp =
                                            EMetCallContextExt::new(&**ExprContextExt::new(
                                                _parentctx.clone(),
                                                _parentState,
                                            ));
                                        recog.push_new_recursion_context(
                                            tmp.clone(),
                                            _startState,
                                            RULE_expr,
                                        );
                                        _localctx = tmp;
                                        recog.base.set_state(274);
                                        if !({ recog.precpred(None, 2) }) {
                                            Err(FailedPredicateError::new(
                                                &mut recog.base,
                                                Some("recog.precpred(None, 2)".to_owned()),
                                                None,
                                            ))?;
                                        }
                                        recog.base.set_state(275);
                                        recog.base.match_token(T__17, &mut recog.err_handler)?;

                                        recog.base.set_state(276);
                                        recog.base.match_token(ID, &mut recog.err_handler)?;

                                        recog.base.set_state(277);
                                        recog.base.match_token(T__2, &mut recog.err_handler)?;

                                        /*InvokeRule args*/
                                        recog.base.set_state(278);
                                        recog.args()?;

                                        recog.base.set_state(279);
                                        recog.base.match_token(T__3, &mut recog.err_handler)?;
                                    }
                                }

                                _ => {}
                            }
                        }
                    }
                    recog.base.set_state(285);
                    recog.err_handler.sync(&mut recog.base)?;
                    _alt = recog.interpreter.adaptive_predict(18, &mut recog.base)?;
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.unroll_recursion_context(_parentctx);

        Ok(_localctx)
    }
}
//------------------- args ----------------
pub type ArgsContextAll<'input> = ArgsContext<'input>;

pub type ArgsContext<'input> = BaseParserRuleContext<'input, ArgsContextExt<'input>>;

#[derive(Clone)]
pub struct ArgsContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ArgsContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ArgsContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_args(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_args(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ArgsContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_args(self);
    }
}

impl<'input> CustomRuleContext<'input> for ArgsContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_args
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_args }
}
antlr_rust::tid! {ArgsContextExt<'a>}

impl<'input> ArgsContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ArgsContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            ArgsContextExt { ph: PhantomData },
        ))
    }
}

pub trait ArgsContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ArgsContextExt<'input>>
{
    fn arg_all(&self) -> Vec<Rc<ArgContextAll<'input>>>
    where
        Self: Sized,
    {
        self.children_of_type()
    }
    fn arg(&self, i: usize) -> Option<Rc<ArgContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(i)
    }
}

impl<'input> ArgsContextAttrs<'input> for ArgsContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn args(&mut self) -> Result<Rc<ArgsContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ArgsContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 34, RULE_args);
        let mut _localctx: Rc<ArgsContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            recog.base.set_state(297);
            recog.err_handler.sync(&mut recog.base)?;
            match recog.interpreter.adaptive_predict(21, &mut recog.base)? {
                1 => {
                    //recog.base.enter_outer_alt(_localctx.clone(), 1);
                    recog.base.enter_outer_alt(None, 1);
                    {
                        recog.base.set_state(294);
                        recog.err_handler.sync(&mut recog.base)?;
                        _la = recog.base.input.la(1);
                        if (((_la) & !0x3f) == 0
                            && ((1usize << _la)
                                & ((1usize << T__2)
                                    | (1usize << T__25)
                                    | (1usize << T__26)
                                    | (1usize << T__29)
                                    | (1usize << T__30)))
                                != 0)
                            || (((_la - 33) & !0x3f) == 0
                                && ((1usize << (_la - 33))
                                    & ((1usize << (T__32 - 33))
                                        | (1usize << (INT - 33))
                                        | (1usize << (ID - 33))
                                        | (1usize << (STR - 33))))
                                    != 0)
                        {
                            {
                                /*InvokeRule arg*/
                                recog.base.set_state(286);
                                recog.arg()?;

                                recog.base.set_state(291);
                                recog.err_handler.sync(&mut recog.base)?;
                                _la = recog.base.input.la(1);
                                while _la == T__4 {
                                    {
                                        {
                                            recog.base.set_state(287);
                                            recog.base.match_token(T__4, &mut recog.err_handler)?;

                                            /*InvokeRule arg*/
                                            recog.base.set_state(288);
                                            recog.arg()?;
                                        }
                                    }
                                    recog.base.set_state(293);
                                    recog.err_handler.sync(&mut recog.base)?;
                                    _la = recog.base.input.la(1);
                                }
                            }
                        }
                    }
                }
                2 => {
                    //recog.base.enter_outer_alt(_localctx.clone(), 2);
                    recog.base.enter_outer_alt(None, 2);
                    {}
                }

                _ => {}
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- arg ----------------
pub type ArgContextAll<'input> = ArgContext<'input>;

pub type ArgContext<'input> = BaseParserRuleContext<'input, ArgContextExt<'input>>;

#[derive(Clone)]
pub struct ArgContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for ArgContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for ArgContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_arg(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_arg(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for ArgContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_arg(self);
    }
}

impl<'input> CustomRuleContext<'input> for ArgContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_arg
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_arg }
}
antlr_rust::tid! {ArgContextExt<'a>}

impl<'input> ArgContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<ArgContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            ArgContextExt { ph: PhantomData },
        ))
    }
}

pub trait ArgContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<ArgContextExt<'input>>
{
    fn expr(&self) -> Option<Rc<ExprContextAll<'input>>>
    where
        Self: Sized,
    {
        self.child_of_type(0)
    }
}

impl<'input> ArgContextAttrs<'input> for ArgContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn arg(&mut self) -> Result<Rc<ArgContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = ArgContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 36, RULE_arg);
        let mut _localctx: Rc<ArgContextAll> = _localctx;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                /*InvokeRule expr*/
                recog.base.set_state(299);
                recog.expr_rec(0)?;
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- addOp ----------------
pub type AddOpContextAll<'input> = AddOpContext<'input>;

pub type AddOpContext<'input> = BaseParserRuleContext<'input, AddOpContextExt<'input>>;

#[derive(Clone)]
pub struct AddOpContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for AddOpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for AddOpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_addOp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_addOp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for AddOpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_addOp(self);
    }
}

impl<'input> CustomRuleContext<'input> for AddOpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_addOp
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_addOp }
}
antlr_rust::tid! {AddOpContextExt<'a>}

impl<'input> AddOpContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<AddOpContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            AddOpContextExt { ph: PhantomData },
        ))
    }
}

pub trait AddOpContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<AddOpContextExt<'input>>
{
}

impl<'input> AddOpContextAttrs<'input> for AddOpContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn addOp(&mut self) -> Result<Rc<AddOpContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = AddOpContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 38, RULE_addOp);
        let mut _localctx: Rc<AddOpContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(301);
                _la = recog.base.input.la(1);
                if { !(_la == T__25 || _la == T__33) } {
                    recog.err_handler.recover_inline(&mut recog.base)?;
                } else {
                    if recog.base.input.la(1) == TOKEN_EOF {
                        recog.base.matched_eof = true
                    };
                    recog.err_handler.report_match(&mut recog.base);
                    recog.base.consume(&mut recog.err_handler);
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- mulOp ----------------
pub type MulOpContextAll<'input> = MulOpContext<'input>;

pub type MulOpContext<'input> = BaseParserRuleContext<'input, MulOpContextExt<'input>>;

#[derive(Clone)]
pub struct MulOpContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for MulOpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for MulOpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_mulOp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_mulOp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for MulOpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_mulOp(self);
    }
}

impl<'input> CustomRuleContext<'input> for MulOpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_mulOp
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_mulOp }
}
antlr_rust::tid! {MulOpContextExt<'a>}

impl<'input> MulOpContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<MulOpContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            MulOpContextExt { ph: PhantomData },
        ))
    }
}

pub trait MulOpContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<MulOpContextExt<'input>>
{
}

impl<'input> MulOpContextAttrs<'input> for MulOpContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn mulOp(&mut self) -> Result<Rc<MulOpContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = MulOpContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 40, RULE_mulOp);
        let mut _localctx: Rc<MulOpContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(303);
                _la = recog.base.input.la(1);
                if {
                    !(((_la - 35) & !0x3f) == 0
                        && ((1usize << (_la - 35))
                            & ((1usize << (T__34 - 35))
                                | (1usize << (T__35 - 35))
                                | (1usize << (T__36 - 35))))
                            != 0)
                } {
                    recog.err_handler.recover_inline(&mut recog.base)?;
                } else {
                    if recog.base.input.la(1) == TOKEN_EOF {
                        recog.base.matched_eof = true
                    };
                    recog.err_handler.report_match(&mut recog.base);
                    recog.base.consume(&mut recog.err_handler);
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}
//------------------- relOp ----------------
pub type RelOpContextAll<'input> = RelOpContext<'input>;

pub type RelOpContext<'input> = BaseParserRuleContext<'input, RelOpContextExt<'input>>;

#[derive(Clone)]
pub struct RelOpContextExt<'input> {
    ph: PhantomData<&'input str>,
}

impl<'input> LatteParserContext<'input> for RelOpContext<'input> {}

impl<'input, 'a> Listenable<dyn LatteListener<'input> + 'a> for RelOpContext<'input> {
    fn enter(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.enter_every_rule(self);
        listener.enter_relOp(self);
    }
    fn exit(&self, listener: &mut (dyn LatteListener<'input> + 'a)) {
        listener.exit_relOp(self);
        listener.exit_every_rule(self);
    }
}

impl<'input, 'a> Visitable<dyn LatteVisitor<'input> + 'a> for RelOpContext<'input> {
    fn accept(&self, visitor: &mut (dyn LatteVisitor<'input> + 'a)) {
        visitor.visit_relOp(self);
    }
}

impl<'input> CustomRuleContext<'input> for RelOpContextExt<'input> {
    type TF = LocalTokenFactory<'input>;
    type Ctx = LatteParserContextType;
    fn get_rule_index(&self) -> usize {
        RULE_relOp
    }
    //fn type_rule_index() -> usize where Self: Sized { RULE_relOp }
}
antlr_rust::tid! {RelOpContextExt<'a>}

impl<'input> RelOpContextExt<'input> {
    fn new(
        parent: Option<Rc<dyn LatteParserContext<'input> + 'input>>,
        invoking_state: isize,
    ) -> Rc<RelOpContextAll<'input>> {
        Rc::new(BaseParserRuleContext::new_parser_ctx(
            parent,
            invoking_state,
            RelOpContextExt { ph: PhantomData },
        ))
    }
}

pub trait RelOpContextAttrs<'input>:
    LatteParserContext<'input> + BorrowMut<RelOpContextExt<'input>>
{
}

impl<'input> RelOpContextAttrs<'input> for RelOpContext<'input> {}

impl<'input, I, H> LatteParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input>> + TidAble<'input>,
    H: ErrorStrategy<'input, BaseParserType<'input, I>>,
{
    pub fn relOp(&mut self) -> Result<Rc<RelOpContextAll<'input>>, ANTLRError> {
        let mut recog = self;
        let _parentctx = recog.ctx.take();
        let mut _localctx = RelOpContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 42, RULE_relOp);
        let mut _localctx: Rc<RelOpContextAll> = _localctx;
        let mut _la: isize = -1;
        let result: Result<(), ANTLRError> = (|| {
            //recog.base.enter_outer_alt(_localctx.clone(), 1);
            recog.base.enter_outer_alt(None, 1);
            {
                recog.base.set_state(305);
                _la = recog.base.input.la(1);
                if {
                    !(((_la - 38) & !0x3f) == 0
                        && ((1usize << (_la - 38))
                            & ((1usize << (T__37 - 38))
                                | (1usize << (T__38 - 38))
                                | (1usize << (T__39 - 38))
                                | (1usize << (T__40 - 38))
                                | (1usize << (T__41 - 38))
                                | (1usize << (T__42 - 38))))
                            != 0)
                } {
                    recog.err_handler.recover_inline(&mut recog.base)?;
                } else {
                    if recog.base.input.la(1) == TOKEN_EOF {
                        recog.base.matched_eof = true
                    };
                    recog.err_handler.report_match(&mut recog.base);
                    recog.base.consume(&mut recog.err_handler);
                }
            }
            Ok(())
        })();
        match result {
            Ok(_) => {}
            Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
            Err(ref re) => {
                //_localctx.exception = re;
                recog.err_handler.report_error(&mut recog.base, re);
                recog.err_handler.recover(&mut recog.base, re)?;
            }
        }
        recog.base.exit_rule();

        Ok(_localctx)
    }
}

lazy_static! {
    static ref _ATN: Arc<ATN> =
        Arc::new(ATNDeserializer::new(None).deserialize(_serializedATN.chars()));
    static ref _decision_to_DFA: Arc<Vec<antlr_rust::RwLock<DFA>>> = {
        let mut dfa = Vec::new();
        let size = _ATN.decision_to_state.len();
        for i in 0..size {
            dfa.push(DFA::new(_ATN.clone(), _ATN.get_decision_state(i), i as isize).into())
        }
        Arc::new(dfa)
    };
}

const _serializedATN: &'static str =
    "\x03\u{608b}\u{a72a}\u{8133}\u{b9ed}\u{417c}\u{3be7}\u{7786}\u{5964}\x03\
	\x33\u{136}\x04\x02\x09\x02\x04\x03\x09\x03\x04\x04\x09\x04\x04\x05\x09\
	\x05\x04\x06\x09\x06\x04\x07\x09\x07\x04\x08\x09\x08\x04\x09\x09\x09\x04\
	\x0a\x09\x0a\x04\x0b\x09\x0b\x04\x0c\x09\x0c\x04\x0d\x09\x0d\x04\x0e\x09\
	\x0e\x04\x0f\x09\x0f\x04\x10\x09\x10\x04\x11\x09\x11\x04\x12\x09\x12\x04\
	\x13\x09\x13\x04\x14\x09\x14\x04\x15\x09\x15\x04\x16\x09\x16\x04\x17\x09\
	\x17\x03\x02\x06\x02\x30\x0a\x02\x0d\x02\x0e\x02\x31\x03\x03\x03\x03\x03\
	\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x05\x03\x3d\x0a\x03\
	\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\x03\x04\x03\x05\x03\x05\
	\x03\x05\x07\x05\x49\x0a\x05\x0c\x05\x0e\x05\x4c\x0b\x05\x03\x05\x05\x05\
	\x4f\x0a\x05\x03\x06\x03\x06\x03\x06\x03\x07\x03\x07\x07\x07\x56\x0a\x07\
	\x0c\x07\x0e\x07\x59\x0b\x07\x03\x07\x03\x07\x03\x08\x03\x08\x05\x08\x5f\
	\x0a\x08\x03\x09\x03\x09\x03\x09\x03\x09\x03\x0a\x03\x0a\x03\x0a\x07\x0a\
	\x68\x0a\x0a\x0c\x0a\x0e\x0a\x6b\x0b\x0a\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x05\x0b\x71\x0a\x0b\x03\x0c\x03\x0c\x07\x0c\x75\x0a\x0c\x0c\x0c\x0e\x0c\
	\x78\x0b\x0c\x03\x0c\x03\x0c\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\
	\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\
	\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\
	\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\
	\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\
	\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\
	\x0d\x03\x0d\x03\x0d\x03\x0d\x05\x0d\u{b2}\x0a\x0d\x03\x0e\x03\x0e\x03\x0e\
	\x03\x0e\x03\x0e\x03\x0e\x03\x0e\x03\x0e\x03\x0e\x03\x0e\x03\x0e\x07\x0e\
	\u{bf}\x0a\x0e\x0c\x0e\x0e\x0e\u{c2}\x0b\x0e\x03\x0f\x03\x0f\x05\x0f\u{c6}\
	\x0a\x0f\x03\x10\x03\x10\x03\x10\x03\x10\x03\x10\x05\x10\u{cd}\x0a\x10\x03\
	\x10\x03\x10\x07\x10\u{d1}\x0a\x10\x0c\x10\x0e\x10\u{d4}\x0b\x10\x03\x11\
	\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x05\x11\
	\u{df}\x0a\x11\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\
	\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\
	\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x05\x12\u{f9}\
	\x0a\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\
	\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\
	\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\
	\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x03\x12\x07\x12\u{11c}\
	\x0a\x12\x0c\x12\x0e\x12\u{11f}\x0b\x12\x03\x13\x03\x13\x03\x13\x07\x13\
	\u{124}\x0a\x13\x0c\x13\x0e\x13\u{127}\x0b\x13\x05\x13\u{129}\x0a\x13\x03\
	\x13\x05\x13\u{12c}\x0a\x13\x03\x14\x03\x14\x03\x15\x03\x15\x03\x16\x03\
	\x16\x03\x17\x03\x17\x03\x17\x02\x05\x1a\x1e\x22\x18\x02\x04\x06\x08\x0a\
	\x0c\x0e\x10\x12\x14\x16\x18\x1a\x1c\x1e\x20\x22\x24\x26\x28\x2a\x2c\x02\
	\x06\x03\x02\x1c\x1d\x04\x02\x1c\x1c\x24\x24\x03\x02\x25\x27\x03\x02\x28\
	\x2d\x02\u{154}\x02\x2f\x03\x02\x02\x02\x04\x3c\x03\x02\x02\x02\x06\x3e\
	\x03\x02\x02\x02\x08\x4e\x03\x02\x02\x02\x0a\x50\x03\x02\x02\x02\x0c\x53\
	\x03\x02\x02\x02\x0e\x5e\x03\x02\x02\x02\x10\x60\x03\x02\x02\x02\x12\x64\
	\x03\x02\x02\x02\x14\x70\x03\x02\x02\x02\x16\x72\x03\x02\x02\x02\x18\u{b1}\
	\x03\x02\x02\x02\x1a\u{b3}\x03\x02\x02\x02\x1c\u{c5}\x03\x02\x02\x02\x1e\
	\u{cc}\x03\x02\x02\x02\x20\u{de}\x03\x02\x02\x02\x22\u{f8}\x03\x02\x02\x02\
	\x24\u{12b}\x03\x02\x02\x02\x26\u{12d}\x03\x02\x02\x02\x28\u{12f}\x03\x02\
	\x02\x02\x2a\u{131}\x03\x02\x02\x02\x2c\u{133}\x03\x02\x02\x02\x2e\x30\x05\
	\x04\x03\x02\x2f\x2e\x03\x02\x02\x02\x30\x31\x03\x02\x02\x02\x31\x2f\x03\
	\x02\x02\x02\x31\x32\x03\x02\x02\x02\x32\x03\x03\x02\x02\x02\x33\x3d\x05\
	\x06\x04\x02\x34\x35\x07\x03\x02\x02\x35\x36\x07\x31\x02\x02\x36\x3d\x05\
	\x0c\x07\x02\x37\x38\x07\x03\x02\x02\x38\x39\x07\x31\x02\x02\x39\x3a\x07\
	\x04\x02\x02\x3a\x3b\x07\x31\x02\x02\x3b\x3d\x05\x0c\x07\x02\x3c\x33\x03\
	\x02\x02\x02\x3c\x34\x03\x02\x02\x02\x3c\x37\x03\x02\x02\x02\x3d\x05\x03\
	\x02\x02\x02\x3e\x3f\x05\x1c\x0f\x02\x3f\x40\x07\x31\x02\x02\x40\x41\x07\
	\x05\x02\x02\x41\x42\x05\x08\x05\x02\x42\x43\x07\x06\x02\x02\x43\x44\x05\
	\x16\x0c\x02\x44\x07\x03\x02\x02\x02\x45\x4a\x05\x0a\x06\x02\x46\x47\x07\
	\x07\x02\x02\x47\x49\x05\x0a\x06\x02\x48\x46\x03\x02\x02\x02\x49\x4c\x03\
	\x02\x02\x02\x4a\x48\x03\x02\x02\x02\x4a\x4b\x03\x02\x02\x02\x4b\x4f\x03\
	\x02\x02\x02\x4c\x4a\x03\x02\x02\x02\x4d\x4f\x03\x02\x02\x02\x4e\x45\x03\
	\x02\x02\x02\x4e\x4d\x03\x02\x02\x02\x4f\x09\x03\x02\x02\x02\x50\x51\x05\
	\x1e\x10\x02\x51\x52\x07\x31\x02\x02\x52\x0b\x03\x02\x02\x02\x53\x57\x07\
	\x08\x02\x02\x54\x56\x05\x0e\x08\x02\x55\x54\x03\x02\x02\x02\x56\x59\x03\
	\x02\x02\x02\x57\x55\x03\x02\x02\x02\x57\x58\x03\x02\x02\x02\x58\x5a\x03\
	\x02\x02\x02\x59\x57\x03\x02\x02\x02\x5a\x5b\x07\x09\x02\x02\x5b\x0d\x03\
	\x02\x02\x02\x5c\x5f\x05\x10\x09\x02\x5d\x5f\x05\x06\x04\x02\x5e\x5c\x03\
	\x02\x02\x02\x5e\x5d\x03\x02\x02\x02\x5f\x0f\x03\x02\x02\x02\x60\x61\x05\
	\x1e\x10\x02\x61\x62\x05\x12\x0a\x02\x62\x63\x07\x0a\x02\x02\x63\x11\x03\
	\x02\x02\x02\x64\x69\x05\x14\x0b\x02\x65\x66\x07\x07\x02\x02\x66\x68\x05\
	\x14\x0b\x02\x67\x65\x03\x02\x02\x02\x68\x6b\x03\x02\x02\x02\x69\x67\x03\
	\x02\x02\x02\x69\x6a\x03\x02\x02\x02\x6a\x13\x03\x02\x02\x02\x6b\x69\x03\
	\x02\x02\x02\x6c\x71\x07\x31\x02\x02\x6d\x6e\x07\x31\x02\x02\x6e\x6f\x07\
	\x0b\x02\x02\x6f\x71\x05\x22\x12\x02\x70\x6c\x03\x02\x02\x02\x70\x6d\x03\
	\x02\x02\x02\x71\x15\x03\x02\x02\x02\x72\x76\x07\x08\x02\x02\x73\x75\x05\
	\x18\x0d\x02\x74\x73\x03\x02\x02\x02\x75\x78\x03\x02\x02\x02\x76\x74\x03\
	\x02\x02\x02\x76\x77\x03\x02\x02\x02\x77\x79\x03\x02\x02\x02\x78\x76\x03\
	\x02\x02\x02\x79\x7a\x07\x09\x02\x02\x7a\x17\x03\x02\x02\x02\x7b\u{b2}\x07\
	\x0a\x02\x02\x7c\u{b2}\x05\x16\x0c\x02\x7d\u{b2}\x05\x10\x09\x02\x7e\x7f\
	\x05\x1a\x0e\x02\x7f\u{80}\x07\x0b\x02\x02\u{80}\u{81}\x05\x22\x12\x02\u{81}\
	\u{82}\x07\x0a\x02\x02\u{82}\u{b2}\x03\x02\x02\x02\u{83}\u{84}\x05\x1a\x0e\
	\x02\u{84}\u{85}\x07\x0c\x02\x02\u{85}\u{86}\x07\x0a\x02\x02\u{86}\u{b2}\
	\x03\x02\x02\x02\u{87}\u{88}\x05\x1a\x0e\x02\u{88}\u{89}\x07\x0d\x02\x02\
	\u{89}\u{8a}\x07\x0a\x02\x02\u{8a}\u{b2}\x03\x02\x02\x02\u{8b}\u{8c}\x07\
	\x0e\x02\x02\u{8c}\u{8d}\x05\x22\x12\x02\u{8d}\u{8e}\x07\x0a\x02\x02\u{8e}\
	\u{b2}\x03\x02\x02\x02\u{8f}\u{90}\x07\x0e\x02\x02\u{90}\u{b2}\x07\x0a\x02\
	\x02\u{91}\u{92}\x07\x0f\x02\x02\u{92}\u{93}\x07\x05\x02\x02\u{93}\u{94}\
	\x05\x22\x12\x02\u{94}\u{95}\x07\x06\x02\x02\u{95}\u{96}\x05\x18\x0d\x02\
	\u{96}\u{b2}\x03\x02\x02\x02\u{97}\u{98}\x07\x0f\x02\x02\u{98}\u{99}\x07\
	\x05\x02\x02\u{99}\u{9a}\x05\x22\x12\x02\u{9a}\u{9b}\x07\x06\x02\x02\u{9b}\
	\u{9c}\x05\x18\x0d\x02\u{9c}\u{9d}\x07\x10\x02\x02\u{9d}\u{9e}\x05\x18\x0d\
	\x02\u{9e}\u{b2}\x03\x02\x02\x02\u{9f}\u{a0}\x07\x11\x02\x02\u{a0}\u{a1}\
	\x07\x05\x02\x02\u{a1}\u{a2}\x05\x22\x12\x02\u{a2}\u{a3}\x07\x06\x02\x02\
	\u{a3}\u{a4}\x05\x18\x0d\x02\u{a4}\u{b2}\x03\x02\x02\x02\u{a5}\u{a6}\x05\
	\x22\x12\x02\u{a6}\u{a7}\x07\x0a\x02\x02\u{a7}\u{b2}\x03\x02\x02\x02\u{a8}\
	\u{a9}\x07\x12\x02\x02\u{a9}\u{aa}\x07\x05\x02\x02\u{aa}\u{ab}\x05\x1e\x10\
	\x02\u{ab}\u{ac}\x07\x31\x02\x02\u{ac}\u{ad}\x07\x13\x02\x02\u{ad}\u{ae}\
	\x05\x22\x12\x02\u{ae}\u{af}\x07\x06\x02\x02\u{af}\u{b0}\x05\x18\x0d\x02\
	\u{b0}\u{b2}\x03\x02\x02\x02\u{b1}\x7b\x03\x02\x02\x02\u{b1}\x7c\x03\x02\
	\x02\x02\u{b1}\x7d\x03\x02\x02\x02\u{b1}\x7e\x03\x02\x02\x02\u{b1}\u{83}\
	\x03\x02\x02\x02\u{b1}\u{87}\x03\x02\x02\x02\u{b1}\u{8b}\x03\x02\x02\x02\
	\u{b1}\u{8f}\x03\x02\x02\x02\u{b1}\u{91}\x03\x02\x02\x02\u{b1}\u{97}\x03\
	\x02\x02\x02\u{b1}\u{9f}\x03\x02\x02\x02\u{b1}\u{a5}\x03\x02\x02\x02\u{b1}\
	\u{a8}\x03\x02\x02\x02\u{b2}\x19\x03\x02\x02\x02\u{b3}\u{b4}\x08\x0e\x01\
	\x02\u{b4}\u{b5}\x07\x31\x02\x02\u{b5}\u{c0}\x03\x02\x02\x02\u{b6}\u{b7}\
	\x0c\x04\x02\x02\u{b7}\u{b8}\x07\x14\x02\x02\u{b8}\u{bf}\x07\x31\x02\x02\
	\u{b9}\u{ba}\x0c\x03\x02\x02\u{ba}\u{bb}\x07\x15\x02\x02\u{bb}\u{bc}\x05\
	\x22\x12\x02\u{bc}\u{bd}\x07\x16\x02\x02\u{bd}\u{bf}\x03\x02\x02\x02\u{be}\
	\u{b6}\x03\x02\x02\x02\u{be}\u{b9}\x03\x02\x02\x02\u{bf}\u{c2}\x03\x02\x02\
	\x02\u{c0}\u{be}\x03\x02\x02\x02\u{c0}\u{c1}\x03\x02\x02\x02\u{c1}\x1b\x03\
	\x02\x02\x02\u{c2}\u{c0}\x03\x02\x02\x02\u{c3}\u{c6}\x07\x17\x02\x02\u{c4}\
	\u{c6}\x05\x1e\x10\x02\u{c5}\u{c3}\x03\x02\x02\x02\u{c5}\u{c4}\x03\x02\x02\
	\x02\u{c6}\x1d\x03\x02\x02\x02\u{c7}\u{c8}\x08\x10\x01\x02\u{c8}\u{cd}\x07\
	\x18\x02\x02\u{c9}\u{cd}\x07\x19\x02\x02\u{ca}\u{cd}\x07\x1a\x02\x02\u{cb}\
	\u{cd}\x07\x31\x02\x02\u{cc}\u{c7}\x03\x02\x02\x02\u{cc}\u{c9}\x03\x02\x02\
	\x02\u{cc}\u{ca}\x03\x02\x02\x02\u{cc}\u{cb}\x03\x02\x02\x02\u{cd}\u{d2}\
	\x03\x02\x02\x02\u{ce}\u{cf}\x0c\x04\x02\x02\u{cf}\u{d1}\x07\x1b\x02\x02\
	\u{d0}\u{ce}\x03\x02\x02\x02\u{d1}\u{d4}\x03\x02\x02\x02\u{d2}\u{d0}\x03\
	\x02\x02\x02\u{d2}\u{d3}\x03\x02\x02\x02\u{d3}\x1f\x03\x02\x02\x02\u{d4}\
	\u{d2}\x03\x02\x02\x02\u{d5}\u{df}\x07\x18\x02\x02\u{d6}\u{df}\x07\x19\x02\
	\x02\u{d7}\u{df}\x07\x1a\x02\x02\u{d8}\u{d9}\x05\x1c\x0f\x02\u{d9}\u{da}\
	\x07\x15\x02\x02\u{da}\u{db}\x05\x22\x12\x02\u{db}\u{dc}\x07\x16\x02\x02\
	\u{dc}\u{df}\x03\x02\x02\x02\u{dd}\u{df}\x07\x31\x02\x02\u{de}\u{d5}\x03\
	\x02\x02\x02\u{de}\u{d6}\x03\x02\x02\x02\u{de}\u{d7}\x03\x02\x02\x02\u{de}\
	\u{d8}\x03\x02\x02\x02\u{de}\u{dd}\x03\x02\x02\x02\u{df}\x21\x03\x02\x02\
	\x02\u{e0}\u{e1}\x08\x12\x01\x02\u{e1}\u{e2}\x09\x02\x02\x02\u{e2}\u{f9}\
	\x05\x22\x12\x14\u{e3}\u{f9}\x07\x31\x02\x02\u{e4}\u{f9}\x07\x30\x02\x02\
	\u{e5}\u{f9}\x07\x20\x02\x02\u{e6}\u{f9}\x07\x21\x02\x02\u{e7}\u{e8}\x07\
	\x31\x02\x02\u{e8}\u{e9}\x07\x05\x02\x02\u{e9}\u{ea}\x05\x24\x13\x02\u{ea}\
	\u{eb}\x07\x06\x02\x02\u{eb}\u{f9}\x03\x02\x02\x02\u{ec}\u{f9}\x07\x33\x02\
	\x02\u{ed}\u{ee}\x07\x05\x02\x02\u{ee}\u{ef}\x05\x1e\x10\x02\u{ef}\u{f0}\
	\x07\x06\x02\x02\u{f0}\u{f1}\x07\x22\x02\x02\u{f1}\u{f9}\x03\x02\x02\x02\
	\u{f2}\u{f3}\x07\x05\x02\x02\u{f3}\u{f4}\x05\x22\x12\x02\u{f4}\u{f5}\x07\
	\x06\x02\x02\u{f5}\u{f9}\x03\x02\x02\x02\u{f6}\u{f7}\x07\x23\x02\x02\u{f7}\
	\u{f9}\x05\x20\x11\x02\u{f8}\u{e0}\x03\x02\x02\x02\u{f8}\u{e3}\x03\x02\x02\
	\x02\u{f8}\u{e4}\x03\x02\x02\x02\u{f8}\u{e5}\x03\x02\x02\x02\u{f8}\u{e6}\
	\x03\x02\x02\x02\u{f8}\u{e7}\x03\x02\x02\x02\u{f8}\u{ec}\x03\x02\x02\x02\
	\u{f8}\u{ed}\x03\x02\x02\x02\u{f8}\u{f2}\x03\x02\x02\x02\u{f8}\u{f6}\x03\
	\x02\x02\x02\u{f9}\u{11d}\x03\x02\x02\x02\u{fa}\u{fb}\x0c\x13\x02\x02\u{fb}\
	\u{fc}\x05\x2a\x16\x02\u{fc}\u{fd}\x05\x22\x12\x14\u{fd}\u{11c}\x03\x02\
	\x02\x02\u{fe}\u{ff}\x0c\x12\x02\x02\u{ff}\u{100}\x05\x28\x15\x02\u{100}\
	\u{101}\x05\x22\x12\x13\u{101}\u{11c}\x03\x02\x02\x02\u{102}\u{103}\x0c\
	\x11\x02\x02\u{103}\u{104}\x05\x2c\x17\x02\u{104}\u{105}\x05\x22\x12\x12\
	\u{105}\u{11c}\x03\x02\x02\x02\u{106}\u{107}\x0c\x10\x02\x02\u{107}\u{108}\
	\x07\x1e\x02\x02\u{108}\u{11c}\x05\x22\x12\x10\u{109}\u{10a}\x0c\x0f\x02\
	\x02\u{10a}\u{10b}\x07\x1f\x02\x02\u{10b}\u{11c}\x05\x22\x12\x0f\u{10c}\
	\u{10d}\x0c\x06\x02\x02\u{10d}\u{10e}\x07\x15\x02\x02\u{10e}\u{10f}\x05\
	\x22\x12\x02\u{10f}\u{110}\x07\x16\x02\x02\u{110}\u{11c}\x03\x02\x02\x02\
	\u{111}\u{112}\x0c\x05\x02\x02\u{112}\u{113}\x07\x14\x02\x02\u{113}\u{11c}\
	\x07\x31\x02\x02\u{114}\u{115}\x0c\x04\x02\x02\u{115}\u{116}\x07\x14\x02\
	\x02\u{116}\u{117}\x07\x31\x02\x02\u{117}\u{118}\x07\x05\x02\x02\u{118}\
	\u{119}\x05\x24\x13\x02\u{119}\u{11a}\x07\x06\x02\x02\u{11a}\u{11c}\x03\
	\x02\x02\x02\u{11b}\u{fa}\x03\x02\x02\x02\u{11b}\u{fe}\x03\x02\x02\x02\u{11b}\
	\u{102}\x03\x02\x02\x02\u{11b}\u{106}\x03\x02\x02\x02\u{11b}\u{109}\x03\
	\x02\x02\x02\u{11b}\u{10c}\x03\x02\x02\x02\u{11b}\u{111}\x03\x02\x02\x02\
	\u{11b}\u{114}\x03\x02\x02\x02\u{11c}\u{11f}\x03\x02\x02\x02\u{11d}\u{11b}\
	\x03\x02\x02\x02\u{11d}\u{11e}\x03\x02\x02\x02\u{11e}\x23\x03\x02\x02\x02\
	\u{11f}\u{11d}\x03\x02\x02\x02\u{120}\u{125}\x05\x26\x14\x02\u{121}\u{122}\
	\x07\x07\x02\x02\u{122}\u{124}\x05\x26\x14\x02\u{123}\u{121}\x03\x02\x02\
	\x02\u{124}\u{127}\x03\x02\x02\x02\u{125}\u{123}\x03\x02\x02\x02\u{125}\
	\u{126}\x03\x02\x02\x02\u{126}\u{129}\x03\x02\x02\x02\u{127}\u{125}\x03\
	\x02\x02\x02\u{128}\u{120}\x03\x02\x02\x02\u{128}\u{129}\x03\x02\x02\x02\
	\u{129}\u{12c}\x03\x02\x02\x02\u{12a}\u{12c}\x03\x02\x02\x02\u{12b}\u{128}\
	\x03\x02\x02\x02\u{12b}\u{12a}\x03\x02\x02\x02\u{12c}\x25\x03\x02\x02\x02\
	\u{12d}\u{12e}\x05\x22\x12\x02\u{12e}\x27\x03\x02\x02\x02\u{12f}\u{130}\
	\x09\x03\x02\x02\u{130}\x29\x03\x02\x02\x02\u{131}\u{132}\x09\x04\x02\x02\
	\u{132}\x2b\x03\x02\x02\x02\u{133}\u{134}\x09\x05\x02\x02\u{134}\x2d\x03\
	\x02\x02\x02\x18\x31\x3c\x4a\x4e\x57\x5e\x69\x70\x76\u{b1}\u{be}\u{c0}\u{c5}\
	\u{cc}\u{d2}\u{de}\u{f8}\u{11b}\u{11d}\u{125}\u{128}\u{12b}";
