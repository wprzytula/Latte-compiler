#[allow(non_snake_case)]
use std::rc::Rc;
use std::{cell::RefCell, num::ParseIntError};

use antlr_rust::{
    parser_rule_context::ParserRuleContext,
    tree::{ParseTree, ParseTreeVisitorCompat},
};
use either::Either;
use enum_as_inner::EnumAsInner;
use smallvec::{smallvec, SmallVec};
use thiserror::Error;

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
    Decl(DataDecl),
    SingleDecl(SingleDecl),
    Block(Block),
    Stmt(Stmt),
    Expr(Expr),
    LVal(LVal),

    BinOp(BinOpType),
    UnOp(UnOpType),

    NewType(NewType),
    DataType(DataType),
    Nonvoid(NonvoidType),

    ClassBlock(ClassBlock),
    ClassItem(ClassItem),

    Default,
}

impl Default for AstElem {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Error)]
pub enum ConversionError {
    #[error("Error occured when parsing integer: {0}")]
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
    /**
     *  Core Latte
     *  */

    /* Program */
    fn visit_program(&mut self, ctx: &ProgramContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let (fun_defs, class_defs) = {
            let mut fun_defs = vec![];
            let mut class_defs = vec![];
            for top_def in
                extract_all(children_return).map(|ast_elem| ast_elem.into_top_def().unwrap())
            {
                match top_def {
                    TopDef::FunDef(fun_def) => fun_defs.push(fun_def),
                    TopDef::ClassDef(class_def) => class_defs.push(class_def),
                }
            }
            (fun_defs, class_defs)
        };
        Self::Return::Program(Program(fun_defs, class_defs))
    }

    /* TopDefs */
    fn visit_TopFnDef(&mut self, ctx: &TopFnDefContext<'input>) -> Self::Return {
        let fun_def = self.visit_children(ctx).into_fun_def().unwrap();
        Self::Return::TopDef(TopDef::FunDef(fun_def))
    }

    fn visit_funDef(&mut self, ctx: &FunDefContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let (ret_type, params, block) = extract_3(children_return);
        let ret_type = ret_type.into_data_type().unwrap();
        let params = params.into_params().unwrap();
        let block = block.into_block().unwrap();
        let name = ctx.ID().unwrap().symbol.text.clone().into_owned().into();

        Self::Return::FunDef(FunDef {
            pos: ctx.start().into(),
            ret_type,
            name,
            params,
            block,
        })
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

    /* Decl */
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

    /* Block */
    fn visit_block(&mut self, ctx: &BlockContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        let stmts = extract_all(children_return)
            .map(|ast_elem| ast_elem.into_stmt().unwrap())
            .collect();
        Self::Return::Block(Block(ctx.start().into(), stmts))
    }

    /* Stmt */
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

    /* LVal */
    fn visit_LID(&mut self, ctx: &LIDContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::LVal(LVal(
            ctx.start().into(),
            LValInner::Id(ctx.get_text().into()),
            RefCell::new(None),
        ))
    }

    fn visit_LFunCall(&mut self, ctx: &LFunCallContext<'input>) -> Self::Return {
        let name = ctx.ID().unwrap().get_text().into();
        let args = self.visit_children(ctx).into_args().unwrap();
        Self::Return::LVal(LVal(
            ctx.start().into(),
            LValInner::FunCall { name, args },
            RefCell::new(None),
        ))
    }

    fn visit_LParen(&mut self, ctx: &LParenContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /* DataType */
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

    fn visit_Int(&mut self, ctx: &IntContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::Nonvoid(NonvoidType::TInt)
    }

    /* Expr */
    fn visit_ELVal(&mut self, ctx: &ELValContext<'input>) -> Self::Return {
        let e_lval = self.visit_children(ctx).into_l_val().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::LVal(Box::new(e_lval)),
            RefCell::new(None),
        ))
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

    fn visit_EParen(&mut self, ctx: &EParenContext<'input>) -> Self::Return {
        let children_return = self.visit_children(ctx);
        children_return.as_expr().unwrap();
        children_return
    }

    fn visit_ETrue(&mut self, ctx: &ETrueContext<'input>) -> Self::Return {
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::BoolLit(true),
            RefCell::new(None),
        ))
    }

    fn visit_EFalse(&mut self, ctx: &EFalseContext<'input>) -> Self::Return {
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::BoolLit(false),
            RefCell::new(None),
        ))
    }

    fn visit_EInt(&mut self, ctx: &EIntContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        let int = ctx.get_text().parse::<Int>();
        match int {
            Ok(int) => Self::Return::Expr(Expr(
                ctx.start().into(),
                ExprInner::IntLit(int),
                RefCell::new(None),
            )), // just to satisfy the type checker
            Err(err) => {
                self.error = Some(ConversionError::ParseInt(err));
                Self::Return::Expr(Expr(
                    ctx.start().into(),
                    ExprInner::IntLit(0),
                    RefCell::new(None),
                ))
                // just to satisfy the type checker
            }
        }
    }

    fn visit_EStr(&mut self, ctx: &EStrContext<'input>) -> Self::Return {
        assert!(self.visit_children(ctx).is_default());
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::StringLit(ctx.get_text()),
            RefCell::new(None),
        ))
    }

    fn visit_ERelOp(&mut self, ctx: &ERelOpContext<'input>) -> Self::Return {
        let (expr1, bin_op_type, expr2) = extract_3(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        let op_type = bin_op_type.into_bin_op().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::BinOp(op_type, Box::new(expr1), Box::new(expr2))),
            RefCell::new(None),
        ))
    }

    fn visit_EUnOp(&mut self, ctx: &EUnOpContext<'input>) -> Self::Return {
        let (un_op_type, expr) = extract_2(self.visit_children(ctx));
        let un_op_type = un_op_type.into_un_op().unwrap();
        let expr = expr.into_expr().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::UnOp(un_op_type, Box::new(expr))),
            RefCell::new(None),
        ))
    }

    fn visit_EMulOp(&mut self, ctx: &EMulOpContext<'input>) -> Self::Return {
        let (expr1, bin_op_type, expr2) = extract_3(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        let op_type = bin_op_type.into_bin_op().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::BinOp(op_type, Box::new(expr1), Box::new(expr2))),
            RefCell::new(None),
        ))
    }

    fn visit_EOr(&mut self, ctx: &EOrContext<'input>) -> Self::Return {
        let (expr1, expr2) = extract_2(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::LogOp(LogOpType::Or, Box::new(expr1), Box::new(expr2))),
            RefCell::new(None),
        ))
    }

    fn visit_EAnd(&mut self, ctx: &EAndContext<'input>) -> Self::Return {
        let (expr1, expr2) = extract_2(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::LogOp(LogOpType::And, Box::new(expr1), Box::new(expr2))),
            RefCell::new(None),
        ))
    }

    fn visit_EAddOp(&mut self, ctx: &EAddOpContext<'input>) -> Self::Return {
        let (expr1, bin_op_type, expr2) = extract_3(self.visit_children(ctx));
        let expr1 = expr1.into_expr().unwrap();
        let expr2 = expr2.into_expr().unwrap();
        let op_type = bin_op_type.into_bin_op().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Op(Op::BinOp(op_type, Box::new(expr1), Box::new(expr2))),
            RefCell::new(None),
        ))
    }

    fn visit_unOp(&mut self, ctx: &UnOpContext<'input>) -> Self::Return {
        let op = match ctx.get_text().chars().next().unwrap() {
            '!' => UnOpType::Not,
            '-' => UnOpType::Neg,
            _ => unreachable!(),
        };
        Self::Return::UnOp(op)
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

    /**
     *  Latte extensions
     *  */
    /* TopDefs */
    fn visit_BaseCls(&mut self, ctx: &BaseClsContext<'input>) -> Self::Return {
        let class_id = ctx.ID().unwrap().get_text().into();
        let class_block = self.visit_children(ctx).into_class_block().unwrap();

        let pos = ctx.start().into();
        Self::Return::TopDef(TopDef::ClassDef(ClassDef {
            pos,
            class: class_id,
            base_class: None,
            class_block,
        }))
    }

    fn visit_DerivCls(&mut self, ctx: &DerivClsContext<'input>) -> Self::Return {
        let class_id = ctx.ID(0).unwrap().get_text().into();
        let base_id = ctx.ID(1).unwrap().get_text().into();
        let class_block = self.visit_children(ctx).into_class_block().unwrap();

        let pos = ctx.start().into();
        Self::Return::TopDef(TopDef::ClassDef(ClassDef {
            pos,
            class: class_id,
            base_class: Some(base_id),
            class_block,
        }))
    }

    /* ClassBlock */
    fn visit_classBlock(&mut self, ctx: &ClassBlockContext<'input>) -> Self::Return {
        let class_items = extract_all(self.visit_children(ctx))
            .map(|ast_elem| ast_elem.into_class_item().unwrap())
            .collect();
        Self::Return::ClassBlock(ClassBlock(class_items))
    }

    /* Object's fields and methods */
    fn visit_Field(&mut self, ctx: &FieldContext<'input>) -> Self::Return {
        let nonvoid = self.visit_children(ctx).into_nonvoid().unwrap();
        let id = ctx.ID().unwrap().get_text().into();
        let pos = ctx.start().into();
        Self::Return::ClassItem(ClassItem::Field(pos, nonvoid, id))
    }

    fn visit_Method(&mut self, ctx: &MethodContext<'input>) -> Self::Return {
        let fun_def = self.visit_children(ctx).into_fun_def().unwrap();
        Self::Return::ClassItem(ClassItem::Method(fun_def))
    }

    /* Array DataTypes */
    fn visit_Class(&mut self, ctx: &ClassContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        Self::Return::Nonvoid(NonvoidType::TClass(id))
    }

    fn visit_IntArr(&mut self, _ctx: &IntArrContext<'input>) -> Self::Return {
        Self::Return::Nonvoid(NonvoidType::TIntArr)
    }

    fn visit_StrArr(&mut self, _ctx: &StrArrContext<'input>) -> Self::Return {
        Self::Return::Nonvoid(NonvoidType::TStringArr)
    }

    fn visit_BooleanArr(&mut self, _ctx: &BooleanArrContext<'input>) -> Self::Return {
        Self::Return::Nonvoid(NonvoidType::TBooleanArr)
    }

    fn visit_ClassArr(&mut self, ctx: &ClassArrContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        Self::Return::Nonvoid(NonvoidType::TClassArr(id))
    }

    /* Array NewTypes */
    fn visit_NClass(&mut self, ctx: &NClassContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        Self::Return::NewType(NewType::TClass(id))
    }

    fn visit_NIntArr(&mut self, ctx: &NIntArrContext<'input>) -> Self::Return {
        let len = self.visit_children(ctx).into_expr().unwrap();
        Self::Return::NewType(NewType::TIntArr(Box::new(len)))
    }

    fn visit_NStrArr(&mut self, ctx: &NStrArrContext<'input>) -> Self::Return {
        let len = self.visit_children(ctx).into_expr().unwrap();
        Self::Return::NewType(NewType::TStringArr(Box::new(len)))
    }

    fn visit_NBooleanArr(&mut self, ctx: &NBooleanArrContext<'input>) -> Self::Return {
        let len = self.visit_children(ctx).into_expr().unwrap();
        Self::Return::NewType(NewType::TBooleanArr(Box::new(len)))
    }

    fn visit_NClassArr(&mut self, ctx: &NClassArrContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        let len = self.visit_children(ctx).into_expr().unwrap();
        Self::Return::NewType(NewType::TClassArr(id, Box::new(len)))
    }

    /* Expr */
    fn visit_ENull(&mut self, ctx: &ENullContext<'input>) -> Self::Return {
        let nonvoid = self.visit_children(ctx).into_nonvoid().unwrap();
        Self::Return::Expr(Expr(
            ctx.start().into(),
            ExprInner::Null(nonvoid),
            RefCell::new(None),
        ))
    }

    /* Stmt */
    fn visit_For(&mut self, ctx: &ForContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        let (nonvoid, iterable, body) = extract_3(self.visit_children(ctx));
        let nonvoid = nonvoid.into_nonvoid().unwrap();
        let iterable = iterable.into_expr().unwrap();
        let body = body.into_stmt().unwrap();

        Self::Return::Stmt(Stmt(
            ctx.start().into(),
            StmtInner::For(nonvoid, id, iterable, Box::new(body)),
        ))
    }

    /* LVal */
    fn visit_LNew(&mut self, ctx: &LNewContext<'input>) -> Self::Return {
        let newtype = self.visit_children(ctx).into_new_type().unwrap();
        Self::Return::LVal(LVal(
            ctx.start().into(),
            LValInner::New(newtype),
            RefCell::new(None),
        ))
    }

    fn visit_LField(&mut self, ctx: &LFieldContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        let lval = self.visit_children(ctx).into_l_val().unwrap();
        Self::Return::LVal(LVal(
            ctx.start().into(),
            LValInner::FieldAccess(Box::new(lval), id),
            RefCell::new(None),
        ))
    }

    fn visit_LArr(&mut self, ctx: &LArrContext<'input>) -> Self::Return {
        let (arr, idx) = extract_2(self.visit_children(ctx));
        let arr = arr.into_l_val().unwrap();
        let idx = idx.into_expr().unwrap();
        Self::Return::LVal(LVal(
            ctx.start().into(),
            LValInner::ArrSub(Box::new(arr), idx),
            RefCell::new(None),
        ))
    }

    fn visit_LMetCall(&mut self, ctx: &LMetCallContext<'input>) -> Self::Return {
        let id = ctx.ID().unwrap().get_text().into();
        let (object, args) = extract_2(self.visit_children(ctx));
        let object = object.into_l_val().unwrap();
        let args = args.into_args().unwrap();
        Self::Return::LVal(LVal(
            ctx.start().into(),
            LValInner::MethodCall {
                object: Box::new(object),
                method_name: id,
                args,
            },
            RefCell::new(None),
        ))
    }
}
