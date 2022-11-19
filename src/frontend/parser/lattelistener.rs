#![allow(nonstandard_style)]
// Generated from src/grammars/ext/Latte.g4 by ANTLR 4.8
use antlr_rust::tree::ParseTreeListener;
use super::latteparser::*;

pub trait LatteListener<'input> : ParseTreeListener<'input,LatteParserContextType>{
/**
 * Enter a parse tree produced by {@link LatteParser#program}.
 * @param ctx the parse tree
 */
fn enter_program(&mut self, _ctx: &ProgramContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#program}.
 * @param ctx the parse tree
 */
fn exit_program(&mut self, _ctx: &ProgramContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code FnDef}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn enter_FnDef(&mut self, _ctx: &FnDefContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code FnDef}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn exit_FnDef(&mut self, _ctx: &FnDefContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code BaseCls}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn enter_BaseCls(&mut self, _ctx: &BaseClsContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code BaseCls}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn exit_BaseCls(&mut self, _ctx: &BaseClsContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DerivCls}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn enter_DerivCls(&mut self, _ctx: &DerivClsContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DerivCls}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn exit_DerivCls(&mut self, _ctx: &DerivClsContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#funDef}.
 * @param ctx the parse tree
 */
fn enter_funDef(&mut self, _ctx: &FunDefContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#funDef}.
 * @param ctx the parse tree
 */
fn exit_funDef(&mut self, _ctx: &FunDefContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#arg}.
 * @param ctx the parse tree
 */
fn enter_arg(&mut self, _ctx: &ArgContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#arg}.
 * @param ctx the parse tree
 */
fn exit_arg(&mut self, _ctx: &ArgContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#classBlock}.
 * @param ctx the parse tree
 */
fn enter_classBlock(&mut self, _ctx: &ClassBlockContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#classBlock}.
 * @param ctx the parse tree
 */
fn exit_classBlock(&mut self, _ctx: &ClassBlockContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Field}
 * labeled alternative in {@link LatteParser#classItem}.
 * @param ctx the parse tree
 */
fn enter_Field(&mut self, _ctx: &FieldContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Field}
 * labeled alternative in {@link LatteParser#classItem}.
 * @param ctx the parse tree
 */
fn exit_Field(&mut self, _ctx: &FieldContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Method}
 * labeled alternative in {@link LatteParser#classItem}.
 * @param ctx the parse tree
 */
fn enter_Method(&mut self, _ctx: &MethodContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Method}
 * labeled alternative in {@link LatteParser#classItem}.
 * @param ctx the parse tree
 */
fn exit_Method(&mut self, _ctx: &MethodContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#decl}.
 * @param ctx the parse tree
 */
fn enter_decl(&mut self, _ctx: &DeclContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#decl}.
 * @param ctx the parse tree
 */
fn exit_decl(&mut self, _ctx: &DeclContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#block}.
 * @param ctx the parse tree
 */
fn enter_block(&mut self, _ctx: &BlockContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#block}.
 * @param ctx the parse tree
 */
fn exit_block(&mut self, _ctx: &BlockContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Empty}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_Empty(&mut self, _ctx: &EmptyContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Empty}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_Empty(&mut self, _ctx: &EmptyContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code BlockStmt}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_BlockStmt(&mut self, _ctx: &BlockStmtContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code BlockStmt}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_BlockStmt(&mut self, _ctx: &BlockStmtContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code VarDecl}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_VarDecl(&mut self, _ctx: &VarDeclContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code VarDecl}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_VarDecl(&mut self, _ctx: &VarDeclContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Ass}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_Ass(&mut self, _ctx: &AssContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Ass}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_Ass(&mut self, _ctx: &AssContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Incr}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_Incr(&mut self, _ctx: &IncrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Incr}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_Incr(&mut self, _ctx: &IncrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Decr}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_Decr(&mut self, _ctx: &DecrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Decr}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_Decr(&mut self, _ctx: &DecrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Ret}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_Ret(&mut self, _ctx: &RetContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Ret}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_Ret(&mut self, _ctx: &RetContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code VRet}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_VRet(&mut self, _ctx: &VRetContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code VRet}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_VRet(&mut self, _ctx: &VRetContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Cond}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_Cond(&mut self, _ctx: &CondContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Cond}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_Cond(&mut self, _ctx: &CondContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code CondElse}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_CondElse(&mut self, _ctx: &CondElseContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code CondElse}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_CondElse(&mut self, _ctx: &CondElseContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code While}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_While(&mut self, _ctx: &WhileContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code While}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_While(&mut self, _ctx: &WhileContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code SExp}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_SExp(&mut self, _ctx: &SExpContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code SExp}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_SExp(&mut self, _ctx: &SExpContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Str}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn enter_Str(&mut self, _ctx: &StrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Str}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn exit_Str(&mut self, _ctx: &StrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Arr}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn enter_Arr(&mut self, _ctx: &ArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Arr}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn exit_Arr(&mut self, _ctx: &ArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Bool}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn enter_Bool(&mut self, _ctx: &BoolContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Bool}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn exit_Bool(&mut self, _ctx: &BoolContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Class}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn enter_Class(&mut self, _ctx: &ClassContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Class}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn exit_Class(&mut self, _ctx: &ClassContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Void}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn enter_Void(&mut self, _ctx: &VoidContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Void}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn exit_Void(&mut self, _ctx: &VoidContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Int}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn enter_Int(&mut self, _ctx: &IntContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Int}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn exit_Int(&mut self, _ctx: &IntContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#item}.
 * @param ctx the parse tree
 */
fn enter_item(&mut self, _ctx: &ItemContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#item}.
 * @param ctx the parse tree
 */
fn exit_item(&mut self, _ctx: &ItemContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EId}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EId(&mut self, _ctx: &EIdContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EId}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EId(&mut self, _ctx: &EIdContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EFunCall}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EFunCall(&mut self, _ctx: &EFunCallContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EFunCall}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EFunCall(&mut self, _ctx: &EFunCallContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ERelOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ERelOp(&mut self, _ctx: &ERelOpContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ERelOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ERelOp(&mut self, _ctx: &ERelOpContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ETrue}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ETrue(&mut self, _ctx: &ETrueContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ETrue}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ETrue(&mut self, _ctx: &ETrueContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EOr}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EOr(&mut self, _ctx: &EOrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EOr}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EOr(&mut self, _ctx: &EOrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EInt}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EInt(&mut self, _ctx: &EIntContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EInt}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EInt(&mut self, _ctx: &EIntContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EUnOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EUnOp(&mut self, _ctx: &EUnOpContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EUnOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EUnOp(&mut self, _ctx: &EUnOpContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EStr}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EStr(&mut self, _ctx: &EStrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EStr}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EStr(&mut self, _ctx: &EStrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EArrSub}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EArrSub(&mut self, _ctx: &EArrSubContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EArrSub}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EArrSub(&mut self, _ctx: &EArrSubContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EMulOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EMulOp(&mut self, _ctx: &EMulOpContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EMulOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EMulOp(&mut self, _ctx: &EMulOpContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EAnd}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EAnd(&mut self, _ctx: &EAndContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EAnd}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EAnd(&mut self, _ctx: &EAndContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EParen}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EParen(&mut self, _ctx: &EParenContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EParen}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EParen(&mut self, _ctx: &EParenContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EFalse}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EFalse(&mut self, _ctx: &EFalseContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EFalse}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EFalse(&mut self, _ctx: &EFalseContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EMetCall}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EMetCall(&mut self, _ctx: &EMetCallContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EMetCall}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EMetCall(&mut self, _ctx: &EMetCallContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EAddOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EAddOp(&mut self, _ctx: &EAddOpContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EAddOp}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EAddOp(&mut self, _ctx: &EAddOpContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ENull}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ENull(&mut self, _ctx: &ENullContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ENull}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ENull(&mut self, _ctx: &ENullContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code EField}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_EField(&mut self, _ctx: &EFieldContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code EField}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_EField(&mut self, _ctx: &EFieldContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#addOp}.
 * @param ctx the parse tree
 */
fn enter_addOp(&mut self, _ctx: &AddOpContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#addOp}.
 * @param ctx the parse tree
 */
fn exit_addOp(&mut self, _ctx: &AddOpContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#mulOp}.
 * @param ctx the parse tree
 */
fn enter_mulOp(&mut self, _ctx: &MulOpContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#mulOp}.
 * @param ctx the parse tree
 */
fn exit_mulOp(&mut self, _ctx: &MulOpContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#relOp}.
 * @param ctx the parse tree
 */
fn enter_relOp(&mut self, _ctx: &RelOpContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#relOp}.
 * @param ctx the parse tree
 */
fn exit_relOp(&mut self, _ctx: &RelOpContext<'input>) { }

}

antlr_rust::coerce_from!{ 'input : LatteListener<'input> }


