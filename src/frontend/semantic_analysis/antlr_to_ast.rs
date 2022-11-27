use std::num::ParseIntError;
#[allow(non_snake_case)]
use std::rc::Rc;

use antlr_rust::{
    parser_rule_context::ParserRuleContext,
    tree::{ParseTree, ParseTreeVisitorCompat},
};
use either::Either;
use enum_as_inner::EnumAsInner;
use smallvec::{smallvec, SmallVec};

use crate::frontend::parser::{latteparser::*, lattevisitor::LatteVisitorCompat};

use super::ast::*;

impl<'input> TryFrom<Rc<ProgramContextAll<'input>>> for Program {
    type Error = ConversionError;

    fn try_from(antlr_program: Rc<ProgramContextAll<'input>>) -> Result<Self, Self::Error> {
        let mut visitor = ConverterVisitor {
            temp_result: Default::default(),
            error: None,
        };
        let program = visitor
            .visit_program(&antlr_program)
            .into_program()
            .unwrap();
        if let Some(err) = visitor.error {
            Err(err)
        } else {
            Ok(program)
        }
    }
}

type ChildrenBufInner = [AstElem; 4];
type ChildrenBuf = Box<SmallVec<ChildrenBufInner>>;

fn extract_2(children: AstElem) -> (AstElem, AstElem) {
    let mut iter = children.into_aggregate().unwrap().into_iter();
    let fst = iter.next().unwrap();
    let snd = iter.next().unwrap();
    iter.next().ok_or(()).unwrap_err();
    (fst, snd)
}

fn extract_3(children: AstElem) -> (AstElem, AstElem, AstElem) {
    let mut iter = children.into_aggregate().unwrap().into_iter();
    let fst = iter.next().unwrap();
    let snd = iter.next().unwrap();
    let trd = iter.next().unwrap();
    iter.next().ok_or(()).unwrap_err();
    (fst, snd, trd)
}

fn extract_4(children: AstElem) -> (AstElem, AstElem, AstElem, AstElem) {
    let mut iter = children.into_aggregate().unwrap().into_iter();
    let fst = iter.next().unwrap();
    let snd = iter.next().unwrap();
    let trd = iter.next().unwrap();
    let frt = iter.next().unwrap();
    iter.next().ok_or(()).unwrap_err();
    (fst, snd, trd, frt)
}

fn extract_all(children: AstElem) -> impl Iterator<Item = AstElem> {
    if let AstElem::Aggregate(aggr) = children {
        Either::Left((*aggr).into_iter())
    } else {
        Either::Right(std::iter::once(children).filter(|child| !child.is_default()))
    }
}

#[derive(Debug, EnumAsInner)]
enum AstElem {
    Aggregate(ChildrenBuf),

    Program(Program),
    TopDef(TopDef),
    FunDef(FunDef),
    Param(Param),
    Params(Vec<Param>),
    Args(Vec<Box<Expr>>),
    ClassBlock(ClassBlock),
    Decl(DataDecl),
    SingleDecl(SingleDecl),
    Block(Block),
    Stmt(Stmt),
    Expr(Expr),
    LVal(LVal),

    BinOp(BinOpType),

    NewType(NewType),
    DataType(DataType),
    Nonvoid(NonvoidType),

    Default,
}

impl Default for AstElem {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug)]
pub enum ConversionError {
    ParseInt(ParseIntError),
}

struct ConversionResult(Result<AstElem, ConversionError>);
impl From<Result<AstElem, ConversionError>> for ConversionResult {
    fn from(r: Result<AstElem, ConversionError>) -> Self {
        Self(r)
    }
}
impl From<ConversionResult> for Result<AstElem, ConversionError> {
    fn from(cr: ConversionResult) -> Self {
        cr.0
    }
}
impl Default for ConversionResult {
    fn default() -> Self {
        Self(Ok(Default::default()))
    }
}

struct ConverterVisitor {
    temp_result: AstElem,
    error: Option<ConversionError>,
}

impl<'input> ParseTreeVisitorCompat<'input> for ConverterVisitor {
    type Node = LatteParserContextType;
    type Return = AstElem;

    fn temp_result(&mut self) -> &mut Self::Return {
        &mut self.temp_result
    }

    fn visit_error_node(
        &mut self,
        _node: &antlr_rust::tree::ErrorNode<'input, Self::Node>,
    ) -> Self::Return {
        panic!("Error node!!!")
    }

    fn aggregate_results(&self, mut aggregate: Self::Return, next: Self::Return) -> Self::Return {
        if let AstElem::Default = aggregate {
            return next;
        }
        if let AstElem::Default = next {
            return aggregate;
        }

        // eprintln!("Aggregating {:#?} with {:#?}", &aggregate, &next);
        match aggregate {
            AstElem::Default => next,
            AstElem::Aggregate(ref mut agg) => {
                assert!(!matches!(next, Self::Return::Aggregate(_)));
                agg.push(next);
                aggregate
            }
            _ => Self::Return::Aggregate(Box::new(smallvec![aggregate, next])),
        }
    }
}

impl<'a, 'input> LatteVisitorCompat<'input> for ConverterVisitor {
    fn visit_program(&mut self, ctx: &ProgramContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let top_defs = extract_all(children_return)
            .map(|ast_elem| ast_elem.into_top_def().unwrap())
            .collect();
        Self::Return::Program(Program(top_defs))
    }

    fn visit_TopFnDef(&mut self, ctx: &TopFnDefContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_BaseCls(&mut self, ctx: &BaseClsContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_DerivCls(&mut self, ctx: &DerivClsContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_funDef(&mut self, ctx: &FunDefContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let (ret_type, params, block) = extract_3(children_return);
        let ret_type = ret_type.into_data_type().unwrap();
        let params = params.into_params().unwrap();
        let block = block.into_block().unwrap();
        let name = ctx.ID().unwrap().symbol.text.clone().into_owned().into();

        Self::Return::TopDef(TopDef::FunDef(FunDef {
            pos: ctx.start().into(),
            ret_type,
            name,
            params,
            block,
        }))
    }

    fn visit_params(&mut self, ctx: &ParamsContext<'input>) -> Self::Return {
        let params = extract_all(self.visit_children(ctx))
            .map(|ast_elem| ast_elem.into_param().unwrap())
            .collect();
        Self::Return::Params(params)
    }

    fn visit_param(&mut self, ctx: &ParamContext<'input>) -> Self::Return {
        let name = ctx.ID().unwrap().get_text().into();
        let nonvoid = self.visit_children(ctx).into_nonvoid().unwrap();
        Self::Return::Param(Param {
            type_: nonvoid,
            name,
        })
    }

    fn visit_classBlock(&mut self, ctx: &ClassBlockContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_Field(&mut self, ctx: &FieldContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_Method(&mut self, ctx: &MethodContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_decl(&mut self, ctx: &DeclContext<'input>) -> Self::Return {
        let mut children = extract_all(self.visit_children(ctx));
        let nonvoid = children.next().unwrap().into_nonvoid().unwrap();
        let items = extract_all(children.next().unwrap())
            .map(|ast_elem| ast_elem.into_single_decl().unwrap())
            .collect();
        children.next().ok_or(()).unwrap_err(); // assert no more items
        Self::Return::Decl(DataDecl {
            type_: nonvoid,
            decls: items,
        })
    }

    fn visit_items(&mut self, ctx: &ItemsContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_DeclItemUninit(&mut self, ctx: &DeclItemUninitContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        Self::Return::SingleDecl(SingleDecl {
            name: id,
            init: None,
        })
    }

    fn visit_DeclItemInit(&mut self, ctx: &DeclItemInitContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        let init = self.visit_children(ctx).into_expr().unwrap();
        Self::Return::SingleDecl(SingleDecl {
            name: id,
            init: Some(init),
        })
    }

    fn visit_block(&mut self, ctx: &BlockContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let stmts = extract_all(children_return)
            .map(|ast_elem| ast_elem.into_stmt().unwrap())
            .collect();
        Self::Return::Block(Block(ctx.start().into(), stmts))
    }

    fn visit_Empty(&mut self, ctx: &EmptyContext<'input>) -> Self::Return {
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::Empty))
    }

    fn visit_BlockStmt(&mut self, ctx: &BlockStmtContext<'input>) -> Self::Return {
        let block = self.visit_children(ctx).into_block().unwrap();
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::Block(block)))
    }

    fn visit_VarDecl(&mut self, ctx: &VarDeclContext<'input>) -> Self::Return {
        let decl = self.visit_children(ctx).into_decl().unwrap();
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::VarDecl(decl)))
    }

    fn visit_Ass(&mut self, ctx: &AssContext<'input>) -> Self::Return {
        let (lval, expr) = extract_2(self.visit_children(ctx));
        let lval = lval.into_l_val().unwrap();
        let expr = expr.into_expr().unwrap();
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::Ass(lval, expr)))
    }

    fn visit_Incr(&mut self, ctx: &IncrContext<'input>) -> Self::Return {
        let lval = self.visit_children(ctx).into_l_val().unwrap();
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::Incr(lval)))
    }

    fn visit_Decr(&mut self, ctx: &DecrContext<'input>) -> Self::Return {
        let lval = self.visit_children(ctx).into_l_val().unwrap();
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::Decr(lval)))
    }

    fn visit_Ret(&mut self, ctx: &RetContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let ret = children_return.into_expr().unwrap();
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::Return(ret)))
    }

    fn visit_VRet(&mut self, ctx: &VRetContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::VoidReturn))
    }

    fn visit_Cond(&mut self, ctx: &CondContext<'input>) -> Self::Return {
        let (cond, then) = extract_2(self.visit_children(ctx));
        let cond = cond.into_expr().unwrap();
        let then = then.into_stmt().unwrap();
        Self::Return::Stmt(Stmt(
            ctx.start().into(),
            StmtInner::Cond(cond, Box::new(then)),
        ))
    }

    fn visit_CondElse(&mut self, ctx: &CondElseContext<'input>) -> Self::Return {
        let (cond, then, else_stmt) = extract_3(self.visit_children(ctx));
        let cond = cond.into_expr().unwrap();
        let then = then.into_stmt().unwrap();
        let else_stmt = else_stmt.into_stmt().unwrap();
        Self::Return::Stmt(Stmt(
            ctx.start().into(),
            StmtInner::CondElse(cond, Box::new(then), Box::new(else_stmt)),
        ))
    }

    fn visit_While(&mut self, ctx: &WhileContext<'input>) -> Self::Return {
        let (cond, body) = extract_2(self.visit_children(ctx));
        let cond = cond.into_expr().unwrap();
        let body = body.into_stmt().unwrap();
        Self::Return::Stmt(Stmt(
            ctx.start().into(),
            StmtInner::While(cond, Box::new(body)),
        ))
    }

    fn visit_SExp(&mut self, ctx: &SExpContext<'input>) -> Self::Return {
        let expr = self.visit_children(ctx).into_expr().unwrap();
        Self::Return::Stmt(Stmt(ctx.start().into(), StmtInner::SExp(expr)))
    }

    fn visit_For(&mut self, ctx: &ForContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_LField(&mut self, ctx: &LFieldContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_LID(&mut self, ctx: &LIDContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::LVal(LVal::Id(ctx.get_text().into()))
    }

    fn visit_LArr(&mut self, ctx: &LArrContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_Void(&mut self, ctx: &VoidContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::DataType(DataType::TVoid)
    }

    fn visit_Nonvoid(&mut self, ctx: &NonvoidContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        if let Self::Return::Nonvoid(nonvoid) = children_return {
            Self::Return::DataType(DataType::Nonvoid(nonvoid))
        } else {
            panic!("Expected nonvoid, got {:?}", children_return)
        }
    }

    fn visit_Str(&mut self, ctx: &StrContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::Nonvoid(NonvoidType::TString)
    }

    fn visit_Bool(&mut self, ctx: &BoolContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::Nonvoid(NonvoidType::TBoolean)
    }

    fn visit_Class(&mut self, ctx: &ClassContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_Int(&mut self, ctx: &IntContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::Nonvoid(NonvoidType::TInt)
    }

    fn visit_NInt(&mut self, ctx: &NIntContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::NewType(NewType::TInt)
    }

    fn visit_NStr(&mut self, ctx: &NStrContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::NewType(NewType::TString)
    }

    fn visit_NBool(&mut self, ctx: &NBoolContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::NewType(NewType::TBoolean)
    }

    fn visit_NClass(&mut self, ctx: &NClassContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_EId(&mut self, ctx: &EIdContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        Self::Return::Expr(Expr(ctx.start().into(), ExprInner::Id(id)))
    }

    fn visit_EFunCall(&mut self, ctx: &EFunCallContext<'input>) -> Self::Return {
        let name = ctx.ID().unwrap().get_text().into();
        let args = self.visit_children(ctx).into_args().unwrap();
        Self::Return::Expr(Expr(ctx.start().into(), ExprInner::FunCall { name, args }))
    }

    fn visit_ERelOp(&mut self, ctx: &ERelOpContext<'input>) -> Self::Return {
        let (expr1, bin_op_type, expr2) = extract_3(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        let op_type = bin_op_type.into_bin_op().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::BinOp(op_type, Box::new(expr1), Box::new(expr2))),
        ))
    }

    fn visit_ETrue(&mut self, ctx: &ETrueContext<'input>) -> Self::Return {
        Self::Return::Expr(Expr(ctx.start().into(), ExprInner::BoolLit(true)))
    }

    fn visit_EOr(&mut self, ctx: &EOrContext<'input>) -> Self::Return {
        let (expr1, expr2) = extract_2(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::LogOp(LogOpType::Or, Box::new(expr1), Box::new(expr2))),
        ))
    }

    fn visit_EInt(&mut self, ctx: &EIntContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        let int = ctx.get_text().parse::<Int>();
        match int {
            Ok(int) => Self::Return::Expr(Expr(ctx.start().into(), ExprInner::IntLit(int))), // just to satisfy the type checker
            Err(err) => {
                self.error = Some(ConversionError::ParseInt(err));
                Self::Return::Expr(Expr(ctx.start().into(), ExprInner::IntLit(0)))
                // just to satisfy the type checker
            }
        }
    }

    fn visit_EUnOp(&mut self, ctx: &EUnOpContext<'input>) -> Self::Return {
        let expr = self.visit_children(ctx).into_expr().unwrap();
        let op = match ctx.get_text().chars().next().unwrap() {
            '!' => UnOpType::Not,
            '-' => UnOpType::Neg,
            _ => unreachable!(),
        };
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::UnOp(op, Box::new(expr))),
        ))
    }

    fn visit_EStr(&mut self, ctx: &EStrContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::StringLit(ctx.get_text()),
        ))
    }

    fn visit_EArrSub(&mut self, ctx: &EArrSubContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_EMulOp(&mut self, ctx: &EMulOpContext<'input>) -> Self::Return {
        let (expr1, bin_op_type, expr2) = extract_3(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        let op_type = bin_op_type.into_bin_op().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::BinOp(op_type, Box::new(expr1), Box::new(expr2))),
        ))
    }

    fn visit_EAnd(&mut self, ctx: &EAndContext<'input>) -> Self::Return {
        let (expr1, expr2) = extract_2(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::LogOp(LogOpType::And, Box::new(expr1), Box::new(expr2))),
        ))
    }

    fn visit_EParen(&mut self, ctx: &EParenContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        children_return.as_expr().unwrap();
        children_return
    }

    fn visit_EFalse(&mut self, ctx: &EFalseContext<'input>) -> Self::Return {
        Self::Return::Expr(Expr(ctx.start().into(), ExprInner::BoolLit(false)))
    }

    fn visit_EMetCall(&mut self, ctx: &EMetCallContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_ENew(&mut self, ctx: &ENewContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_EAddOp(&mut self, ctx: &EAddOpContext<'input>) -> Self::Return {
        let (expr1, bin_op_type, expr2) = extract_3(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        let op_type = bin_op_type.into_bin_op().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::BinOp(op_type, Box::new(expr1), Box::new(expr2))),
        ))
    }

    fn visit_ENull(&mut self, ctx: &ENullContext<'input>) -> Self::Return {
        let nonvoid = self.visit_children(ctx).into_nonvoid().unwrap();
        Self::Return::Expr(Expr(ctx.start().into(), ExprInner::Null(nonvoid)))
    }

    fn visit_EField(&mut self, ctx: &EFieldContext<'input>) -> Self::Return {
        todo!()
    }

    fn visit_args(&mut self, ctx: &ArgsContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let args = extract_all(children_return)
            .map(|ast_elem| Box::new(ast_elem.into_expr().unwrap()))
            .collect();
        Self::Return::Args(args)
    }

    fn visit_arg(&mut self, ctx: &ArgContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        children_return.as_expr().unwrap();
        children_return
    }

    fn visit_addOp(&mut self, ctx: &AddOpContext<'input>) -> Self::Return {
        let op = match ctx.get_text().chars().next().unwrap() {
            '+' => BinOpType::Add,
            '-' => BinOpType::IntOp(IntOpType::IntRet(IntRetType::Sub)),
            _ => unreachable!(),
        };
        Self::Return::BinOp(op)
    }

    fn visit_mulOp(&mut self, ctx: &MulOpContext<'input>) -> Self::Return {
        let op = match ctx.get_text().chars().next().unwrap() {
            '*' => IntRetType::Mul,
            '/' => IntRetType::Div,
            '%' => IntRetType::Mod,
            _ => unreachable!(),
        };
        Self::Return::BinOp(BinOpType::IntOp(IntOpType::IntRet(op)))
    }

    fn visit_relOp(&mut self, ctx: &RelOpContext<'input>) -> Self::Return {
        let op = match ctx.get_text().as_str() {
            ">" => BinOpType::IntOp(IntOpType::BoolRet(BoolRetType::Gt)),
            ">=" => BinOpType::IntOp(IntOpType::BoolRet(BoolRetType::Ge)),
            "<" => BinOpType::IntOp(IntOpType::BoolRet(BoolRetType::Lt)),
            "<=" => BinOpType::IntOp(IntOpType::BoolRet(BoolRetType::Le)),
            "==" => BinOpType::Eq,
            "!=" => BinOpType::NEq,
            _ => unreachable!(),
        };
        Self::Return::BinOp(op)
    }

    fn visit_IntArr(&mut self, ctx: &IntArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_StrArr(&mut self, ctx: &StrArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_BooleanArr(&mut self, ctx: &BooleanArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_ClassArr(&mut self, ctx: &ClassArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_NIntArr(&mut self, ctx: &NIntArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_NStrArr(&mut self, ctx: &NStrArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_NBooleanArr(&mut self, ctx: &NBooleanArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    fn visit_NClassArr(&mut self, ctx: &NClassArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }
}
