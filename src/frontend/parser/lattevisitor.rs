#![allow(nonstandard_style)]
// Generated from src/grammars/ext/Latte.g4 by ANTLR 4.8
use super::latteparser::*;
use antlr_rust::tree::{ParseTreeVisitor, ParseTreeVisitorCompat};

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link LatteParser}.
 */
pub trait LatteVisitor<'input>: ParseTreeVisitor<'input, LatteParserContextType> {
    /**
     * Visit a parse tree produced by {@link LatteParser#program}.
     * @param ctx the parse tree
     */
    fn visit_program(&mut self, ctx: &ProgramContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code TopFnDef}
     * labeled alternative in {@link LatteParser#topDef}.
     * @param ctx the parse tree
     */
    fn visit_TopFnDef(&mut self, ctx: &TopFnDefContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code BaseCls}
     * labeled alternative in {@link LatteParser#topDef}.
     * @param ctx the parse tree
     */
    fn visit_BaseCls(&mut self, ctx: &BaseClsContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code DerivCls}
     * labeled alternative in {@link LatteParser#topDef}.
     * @param ctx the parse tree
     */
    fn visit_DerivCls(&mut self, ctx: &DerivClsContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#funDef}.
     * @param ctx the parse tree
     */
    fn visit_funDef(&mut self, ctx: &FunDefContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#params}.
     * @param ctx the parse tree
     */
    fn visit_params(&mut self, ctx: &ParamsContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#param}.
     * @param ctx the parse tree
     */
    fn visit_param(&mut self, ctx: &ParamContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#classBlock}.
     * @param ctx the parse tree
     */
    fn visit_classBlock(&mut self, ctx: &ClassBlockContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Field}
     * labeled alternative in {@link LatteParser#classItem}.
     * @param ctx the parse tree
     */
    fn visit_Field(&mut self, ctx: &FieldContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Method}
     * labeled alternative in {@link LatteParser#classItem}.
     * @param ctx the parse tree
     */
    fn visit_Method(&mut self, ctx: &MethodContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#decl}.
     * @param ctx the parse tree
     */
    fn visit_decl(&mut self, ctx: &DeclContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#items}.
     * @param ctx the parse tree
     */
    fn visit_items(&mut self, ctx: &ItemsContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code DeclItemUninit}
     * labeled alternative in {@link LatteParser#item}.
     * @param ctx the parse tree
     */
    fn visit_DeclItemUninit(&mut self, ctx: &DeclItemUninitContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code DeclItemInit}
     * labeled alternative in {@link LatteParser#item}.
     * @param ctx the parse tree
     */
    fn visit_DeclItemInit(&mut self, ctx: &DeclItemInitContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#block}.
     * @param ctx the parse tree
     */
    fn visit_block(&mut self, ctx: &BlockContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Empty}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Empty(&mut self, ctx: &EmptyContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code BlockStmt}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_BlockStmt(&mut self, ctx: &BlockStmtContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code VarDecl}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_VarDecl(&mut self, ctx: &VarDeclContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Ass}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Ass(&mut self, ctx: &AssContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Incr}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Incr(&mut self, ctx: &IncrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Decr}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Decr(&mut self, ctx: &DecrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Ret}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Ret(&mut self, ctx: &RetContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code VRet}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_VRet(&mut self, ctx: &VRetContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Cond}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Cond(&mut self, ctx: &CondContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code CondElse}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_CondElse(&mut self, ctx: &CondElseContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code While}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_While(&mut self, ctx: &WhileContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code For}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_For(&mut self, ctx: &ForContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code SExp}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_SExp(&mut self, ctx: &SExpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LField}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LField(&mut self, ctx: &LFieldContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LFunCall}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LFunCall(&mut self, ctx: &LFunCallContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LID}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LID(&mut self, ctx: &LIDContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LArr}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LArr(&mut self, ctx: &LArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LNew}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LNew(&mut self, ctx: &LNewContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LParen}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LParen(&mut self, ctx: &LParenContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LMetCall}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LMetCall(&mut self, ctx: &LMetCallContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Void}
     * labeled alternative in {@link LatteParser#type_}.
     * @param ctx the parse tree
     */
    fn visit_Void(&mut self, ctx: &VoidContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Nonvoid}
     * labeled alternative in {@link LatteParser#type_}.
     * @param ctx the parse tree
     */
    fn visit_Nonvoid(&mut self, ctx: &NonvoidContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code IntArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_IntArr(&mut self, ctx: &IntArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code StrArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_StrArr(&mut self, ctx: &StrArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code BooleanArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_BooleanArr(&mut self, ctx: &BooleanArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ClassArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_ClassArr(&mut self, ctx: &ClassArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Int}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Int(&mut self, ctx: &IntContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Str}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Str(&mut self, ctx: &StrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Bool}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Bool(&mut self, ctx: &BoolContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Class}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Class(&mut self, ctx: &ClassContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NIntArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NIntArr(&mut self, ctx: &NIntArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NStrArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NStrArr(&mut self, ctx: &NStrArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NBooleanArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NBooleanArr(&mut self, ctx: &NBooleanArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NClassArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NClassArr(&mut self, ctx: &NClassArrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NInt}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NInt(&mut self, ctx: &NIntContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NStr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NStr(&mut self, ctx: &NStrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NBool}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NBool(&mut self, ctx: &NBoolContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NClass}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NClass(&mut self, ctx: &NClassContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ERelOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ERelOp(&mut self, ctx: &ERelOpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ETrue}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ETrue(&mut self, ctx: &ETrueContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EOr}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EOr(&mut self, ctx: &EOrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EInt}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EInt(&mut self, ctx: &EIntContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ELVal}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ELVal(&mut self, ctx: &ELValContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EUnOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EUnOp(&mut self, ctx: &EUnOpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EStr}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EStr(&mut self, ctx: &EStrContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EMulOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EMulOp(&mut self, ctx: &EMulOpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EAnd}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EAnd(&mut self, ctx: &EAndContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EParen}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EParen(&mut self, ctx: &EParenContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EFalse}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EFalse(&mut self, ctx: &EFalseContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EAddOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EAddOp(&mut self, ctx: &EAddOpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ENull}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ENull(&mut self, ctx: &ENullContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#args}.
     * @param ctx the parse tree
     */
    fn visit_args(&mut self, ctx: &ArgsContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#arg}.
     * @param ctx the parse tree
     */
    fn visit_arg(&mut self, ctx: &ArgContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#unOp}.
     * @param ctx the parse tree
     */
    fn visit_unOp(&mut self, ctx: &UnOpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#addOp}.
     * @param ctx the parse tree
     */
    fn visit_addOp(&mut self, ctx: &AddOpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#mulOp}.
     * @param ctx the parse tree
     */
    fn visit_mulOp(&mut self, ctx: &MulOpContext<'input>) {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#relOp}.
     * @param ctx the parse tree
     */
    fn visit_relOp(&mut self, ctx: &RelOpContext<'input>) {
        self.visit_children(ctx)
    }
}

pub trait LatteVisitorCompat<'input>:
    ParseTreeVisitorCompat<'input, Node = LatteParserContextType>
{
    /**
     * Visit a parse tree produced by {@link LatteParser#program}.
     * @param ctx the parse tree
     */
    fn visit_program(&mut self, ctx: &ProgramContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code TopFnDef}
     * labeled alternative in {@link LatteParser#topDef}.
     * @param ctx the parse tree
     */
    fn visit_TopFnDef(&mut self, ctx: &TopFnDefContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code BaseCls}
     * labeled alternative in {@link LatteParser#topDef}.
     * @param ctx the parse tree
     */
    fn visit_BaseCls(&mut self, ctx: &BaseClsContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code DerivCls}
     * labeled alternative in {@link LatteParser#topDef}.
     * @param ctx the parse tree
     */
    fn visit_DerivCls(&mut self, ctx: &DerivClsContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#funDef}.
     * @param ctx the parse tree
     */
    fn visit_funDef(&mut self, ctx: &FunDefContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#params}.
     * @param ctx the parse tree
     */
    fn visit_params(&mut self, ctx: &ParamsContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#param}.
     * @param ctx the parse tree
     */
    fn visit_param(&mut self, ctx: &ParamContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#classBlock}.
     * @param ctx the parse tree
     */
    fn visit_classBlock(&mut self, ctx: &ClassBlockContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Field}
     * labeled alternative in {@link LatteParser#classItem}.
     * @param ctx the parse tree
     */
    fn visit_Field(&mut self, ctx: &FieldContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Method}
     * labeled alternative in {@link LatteParser#classItem}.
     * @param ctx the parse tree
     */
    fn visit_Method(&mut self, ctx: &MethodContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#decl}.
     * @param ctx the parse tree
     */
    fn visit_decl(&mut self, ctx: &DeclContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#items}.
     * @param ctx the parse tree
     */
    fn visit_items(&mut self, ctx: &ItemsContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code DeclItemUninit}
     * labeled alternative in {@link LatteParser#item}.
     * @param ctx the parse tree
     */
    fn visit_DeclItemUninit(&mut self, ctx: &DeclItemUninitContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code DeclItemInit}
     * labeled alternative in {@link LatteParser#item}.
     * @param ctx the parse tree
     */
    fn visit_DeclItemInit(&mut self, ctx: &DeclItemInitContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#block}.
     * @param ctx the parse tree
     */
    fn visit_block(&mut self, ctx: &BlockContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Empty}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Empty(&mut self, ctx: &EmptyContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code BlockStmt}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_BlockStmt(&mut self, ctx: &BlockStmtContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code VarDecl}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_VarDecl(&mut self, ctx: &VarDeclContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Ass}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Ass(&mut self, ctx: &AssContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Incr}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Incr(&mut self, ctx: &IncrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Decr}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Decr(&mut self, ctx: &DecrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Ret}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Ret(&mut self, ctx: &RetContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code VRet}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_VRet(&mut self, ctx: &VRetContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Cond}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_Cond(&mut self, ctx: &CondContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code CondElse}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_CondElse(&mut self, ctx: &CondElseContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code While}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_While(&mut self, ctx: &WhileContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code For}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_For(&mut self, ctx: &ForContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code SExp}
     * labeled alternative in {@link LatteParser#stmt}.
     * @param ctx the parse tree
     */
    fn visit_SExp(&mut self, ctx: &SExpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LField}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LField(&mut self, ctx: &LFieldContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LFunCall}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LFunCall(&mut self, ctx: &LFunCallContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LID}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LID(&mut self, ctx: &LIDContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LArr}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LArr(&mut self, ctx: &LArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LNew}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LNew(&mut self, ctx: &LNewContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LParen}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LParen(&mut self, ctx: &LParenContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code LMetCall}
     * labeled alternative in {@link LatteParser#lval}.
     * @param ctx the parse tree
     */
    fn visit_LMetCall(&mut self, ctx: &LMetCallContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Void}
     * labeled alternative in {@link LatteParser#type_}.
     * @param ctx the parse tree
     */
    fn visit_Void(&mut self, ctx: &VoidContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Nonvoid}
     * labeled alternative in {@link LatteParser#type_}.
     * @param ctx the parse tree
     */
    fn visit_Nonvoid(&mut self, ctx: &NonvoidContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code IntArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_IntArr(&mut self, ctx: &IntArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code StrArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_StrArr(&mut self, ctx: &StrArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code BooleanArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_BooleanArr(&mut self, ctx: &BooleanArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ClassArr}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_ClassArr(&mut self, ctx: &ClassArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Int}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Int(&mut self, ctx: &IntContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Str}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Str(&mut self, ctx: &StrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Bool}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Bool(&mut self, ctx: &BoolContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code Class}
     * labeled alternative in {@link LatteParser#nonvoid_type}.
     * @param ctx the parse tree
     */
    fn visit_Class(&mut self, ctx: &ClassContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NIntArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NIntArr(&mut self, ctx: &NIntArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NStrArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NStrArr(&mut self, ctx: &NStrArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NBooleanArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NBooleanArr(&mut self, ctx: &NBooleanArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NClassArr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NClassArr(&mut self, ctx: &NClassArrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NInt}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NInt(&mut self, ctx: &NIntContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NStr}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NStr(&mut self, ctx: &NStrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NBool}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NBool(&mut self, ctx: &NBoolContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code NClass}
     * labeled alternative in {@link LatteParser#newtype}.
     * @param ctx the parse tree
     */
    fn visit_NClass(&mut self, ctx: &NClassContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ERelOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ERelOp(&mut self, ctx: &ERelOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ETrue}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ETrue(&mut self, ctx: &ETrueContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EOr}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EOr(&mut self, ctx: &EOrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EInt}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EInt(&mut self, ctx: &EIntContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ELVal}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ELVal(&mut self, ctx: &ELValContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EUnOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EUnOp(&mut self, ctx: &EUnOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EStr}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EStr(&mut self, ctx: &EStrContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EMulOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EMulOp(&mut self, ctx: &EMulOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EAnd}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EAnd(&mut self, ctx: &EAndContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EParen}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EParen(&mut self, ctx: &EParenContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EFalse}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EFalse(&mut self, ctx: &EFalseContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code EAddOp}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_EAddOp(&mut self, ctx: &EAddOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by the {@code ENull}
     * labeled alternative in {@link LatteParser#expr}.
     * @param ctx the parse tree
     */
    fn visit_ENull(&mut self, ctx: &ENullContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#args}.
     * @param ctx the parse tree
     */
    fn visit_args(&mut self, ctx: &ArgsContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#arg}.
     * @param ctx the parse tree
     */
    fn visit_arg(&mut self, ctx: &ArgContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#unOp}.
     * @param ctx the parse tree
     */
    fn visit_unOp(&mut self, ctx: &UnOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#addOp}.
     * @param ctx the parse tree
     */
    fn visit_addOp(&mut self, ctx: &AddOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#mulOp}.
     * @param ctx the parse tree
     */
    fn visit_mulOp(&mut self, ctx: &MulOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }

    /**
     * Visit a parse tree produced by {@link LatteParser#relOp}.
     * @param ctx the parse tree
     */
    fn visit_relOp(&mut self, ctx: &RelOpContext<'input>) -> Self::Return {
        self.visit_children(ctx)
    }
}

impl<'input, T> LatteVisitor<'input> for T
where
    T: LatteVisitorCompat<'input>,
{
    fn visit_program(&mut self, ctx: &ProgramContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_program(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_TopFnDef(&mut self, ctx: &TopFnDefContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_TopFnDef(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_BaseCls(&mut self, ctx: &BaseClsContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_BaseCls(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_DerivCls(&mut self, ctx: &DerivClsContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_DerivCls(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_funDef(&mut self, ctx: &FunDefContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_funDef(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_params(&mut self, ctx: &ParamsContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_params(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_param(&mut self, ctx: &ParamContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_param(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_classBlock(&mut self, ctx: &ClassBlockContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_classBlock(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Field(&mut self, ctx: &FieldContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Field(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Method(&mut self, ctx: &MethodContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Method(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_decl(&mut self, ctx: &DeclContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_decl(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_items(&mut self, ctx: &ItemsContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_items(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_DeclItemUninit(&mut self, ctx: &DeclItemUninitContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_DeclItemUninit(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_DeclItemInit(&mut self, ctx: &DeclItemInitContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_DeclItemInit(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_block(&mut self, ctx: &BlockContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_block(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Empty(&mut self, ctx: &EmptyContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Empty(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_BlockStmt(&mut self, ctx: &BlockStmtContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_BlockStmt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_VarDecl(&mut self, ctx: &VarDeclContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_VarDecl(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Ass(&mut self, ctx: &AssContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Ass(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Incr(&mut self, ctx: &IncrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Incr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Decr(&mut self, ctx: &DecrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Decr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Ret(&mut self, ctx: &RetContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Ret(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_VRet(&mut self, ctx: &VRetContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_VRet(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Cond(&mut self, ctx: &CondContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Cond(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_CondElse(&mut self, ctx: &CondElseContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_CondElse(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_While(&mut self, ctx: &WhileContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_While(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_For(&mut self, ctx: &ForContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_For(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_SExp(&mut self, ctx: &SExpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_SExp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_LField(&mut self, ctx: &LFieldContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_LField(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_LFunCall(&mut self, ctx: &LFunCallContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_LFunCall(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_LID(&mut self, ctx: &LIDContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_LID(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_LArr(&mut self, ctx: &LArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_LArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_LNew(&mut self, ctx: &LNewContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_LNew(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_LParen(&mut self, ctx: &LParenContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_LParen(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_LMetCall(&mut self, ctx: &LMetCallContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_LMetCall(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Void(&mut self, ctx: &VoidContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Void(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Nonvoid(&mut self, ctx: &NonvoidContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Nonvoid(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_IntArr(&mut self, ctx: &IntArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_IntArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_StrArr(&mut self, ctx: &StrArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_StrArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_BooleanArr(&mut self, ctx: &BooleanArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_BooleanArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_ClassArr(&mut self, ctx: &ClassArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_ClassArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Int(&mut self, ctx: &IntContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Int(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Str(&mut self, ctx: &StrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Str(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Bool(&mut self, ctx: &BoolContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Bool(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_Class(&mut self, ctx: &ClassContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_Class(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NIntArr(&mut self, ctx: &NIntArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NIntArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NStrArr(&mut self, ctx: &NStrArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NStrArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NBooleanArr(&mut self, ctx: &NBooleanArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NBooleanArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NClassArr(&mut self, ctx: &NClassArrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NClassArr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NInt(&mut self, ctx: &NIntContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NInt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NStr(&mut self, ctx: &NStrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NStr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NBool(&mut self, ctx: &NBoolContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NBool(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_NClass(&mut self, ctx: &NClassContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_NClass(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_ERelOp(&mut self, ctx: &ERelOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_ERelOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_ETrue(&mut self, ctx: &ETrueContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_ETrue(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EOr(&mut self, ctx: &EOrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EOr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EInt(&mut self, ctx: &EIntContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EInt(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_ELVal(&mut self, ctx: &ELValContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_ELVal(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EUnOp(&mut self, ctx: &EUnOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EUnOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EStr(&mut self, ctx: &EStrContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EStr(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EMulOp(&mut self, ctx: &EMulOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EMulOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EAnd(&mut self, ctx: &EAndContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EAnd(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EParen(&mut self, ctx: &EParenContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EParen(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EFalse(&mut self, ctx: &EFalseContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EFalse(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_EAddOp(&mut self, ctx: &EAddOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_EAddOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_ENull(&mut self, ctx: &ENullContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_ENull(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_args(&mut self, ctx: &ArgsContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_args(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_arg(&mut self, ctx: &ArgContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_arg(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_unOp(&mut self, ctx: &UnOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_unOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_addOp(&mut self, ctx: &AddOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_addOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_mulOp(&mut self, ctx: &MulOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_mulOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }

    fn visit_relOp(&mut self, ctx: &RelOpContext<'input>) {
        let result = <Self as LatteVisitorCompat>::visit_relOp(self, ctx);
        *<Self as ParseTreeVisitorCompat>::temp_result(self) = result;
    }
}
