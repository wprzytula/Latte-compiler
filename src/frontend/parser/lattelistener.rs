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
 * Enter a parse tree produced by the {@code TopFnDef}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn enter_TopFnDef(&mut self, _ctx: &TopFnDefContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TopFnDef}
 * labeled alternative in {@link LatteParser#topDef}.
 * @param ctx the parse tree
 */
fn exit_TopFnDef(&mut self, _ctx: &TopFnDefContext<'input>) { }
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
 * Enter a parse tree produced by {@link LatteParser#params}.
 * @param ctx the parse tree
 */
fn enter_params(&mut self, _ctx: &ParamsContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#params}.
 * @param ctx the parse tree
 */
fn exit_params(&mut self, _ctx: &ParamsContext<'input>) { }
/**
 * Enter a parse tree produced by {@link LatteParser#param}.
 * @param ctx the parse tree
 */
fn enter_param(&mut self, _ctx: &ParamContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#param}.
 * @param ctx the parse tree
 */
fn exit_param(&mut self, _ctx: &ParamContext<'input>) { }
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
 * Enter a parse tree produced by {@link LatteParser#items}.
 * @param ctx the parse tree
 */
fn enter_items(&mut self, _ctx: &ItemsContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#items}.
 * @param ctx the parse tree
 */
fn exit_items(&mut self, _ctx: &ItemsContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DeclItemUninit}
 * labeled alternative in {@link LatteParser#item}.
 * @param ctx the parse tree
 */
fn enter_DeclItemUninit(&mut self, _ctx: &DeclItemUninitContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DeclItemUninit}
 * labeled alternative in {@link LatteParser#item}.
 * @param ctx the parse tree
 */
fn exit_DeclItemUninit(&mut self, _ctx: &DeclItemUninitContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DeclItemInit}
 * labeled alternative in {@link LatteParser#item}.
 * @param ctx the parse tree
 */
fn enter_DeclItemInit(&mut self, _ctx: &DeclItemInitContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DeclItemInit}
 * labeled alternative in {@link LatteParser#item}.
 * @param ctx the parse tree
 */
fn exit_DeclItemInit(&mut self, _ctx: &DeclItemInitContext<'input>) { }
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
 * Enter a parse tree produced by the {@code For}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn enter_For(&mut self, _ctx: &ForContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code For}
 * labeled alternative in {@link LatteParser#stmt}.
 * @param ctx the parse tree
 */
fn exit_For(&mut self, _ctx: &ForContext<'input>) { }
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
 * Enter a parse tree produced by the {@code LField}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn enter_LField(&mut self, _ctx: &LFieldContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LField}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn exit_LField(&mut self, _ctx: &LFieldContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LFunCall}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn enter_LFunCall(&mut self, _ctx: &LFunCallContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LFunCall}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn exit_LFunCall(&mut self, _ctx: &LFunCallContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LID}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn enter_LID(&mut self, _ctx: &LIDContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LID}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn exit_LID(&mut self, _ctx: &LIDContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LArr}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn enter_LArr(&mut self, _ctx: &LArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LArr}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn exit_LArr(&mut self, _ctx: &LArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LNew}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn enter_LNew(&mut self, _ctx: &LNewContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LNew}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn exit_LNew(&mut self, _ctx: &LNewContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LParen}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn enter_LParen(&mut self, _ctx: &LParenContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LParen}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn exit_LParen(&mut self, _ctx: &LParenContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LMetCall}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn enter_LMetCall(&mut self, _ctx: &LMetCallContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LMetCall}
 * labeled alternative in {@link LatteParser#lval}.
 * @param ctx the parse tree
 */
fn exit_LMetCall(&mut self, _ctx: &LMetCallContext<'input>) { }
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
 * Enter a parse tree produced by the {@code Nonvoid}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn enter_Nonvoid(&mut self, _ctx: &NonvoidContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Nonvoid}
 * labeled alternative in {@link LatteParser#type_}.
 * @param ctx the parse tree
 */
fn exit_Nonvoid(&mut self, _ctx: &NonvoidContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code IntArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_IntArr(&mut self, _ctx: &IntArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code IntArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_IntArr(&mut self, _ctx: &IntArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code StrArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_StrArr(&mut self, _ctx: &StrArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code StrArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_StrArr(&mut self, _ctx: &StrArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code BooleanArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_BooleanArr(&mut self, _ctx: &BooleanArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code BooleanArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_BooleanArr(&mut self, _ctx: &BooleanArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ClassArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_ClassArr(&mut self, _ctx: &ClassArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ClassArr}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_ClassArr(&mut self, _ctx: &ClassArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Int}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_Int(&mut self, _ctx: &IntContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Int}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_Int(&mut self, _ctx: &IntContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Str}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_Str(&mut self, _ctx: &StrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Str}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_Str(&mut self, _ctx: &StrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Bool}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_Bool(&mut self, _ctx: &BoolContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Bool}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_Bool(&mut self, _ctx: &BoolContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Class}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn enter_Class(&mut self, _ctx: &ClassContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Class}
 * labeled alternative in {@link LatteParser#nonvoid_type}.
 * @param ctx the parse tree
 */
fn exit_Class(&mut self, _ctx: &ClassContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NIntArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NIntArr(&mut self, _ctx: &NIntArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NIntArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NIntArr(&mut self, _ctx: &NIntArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NStrArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NStrArr(&mut self, _ctx: &NStrArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NStrArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NStrArr(&mut self, _ctx: &NStrArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NBooleanArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NBooleanArr(&mut self, _ctx: &NBooleanArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NBooleanArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NBooleanArr(&mut self, _ctx: &NBooleanArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NClassArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NClassArr(&mut self, _ctx: &NClassArrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NClassArr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NClassArr(&mut self, _ctx: &NClassArrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NInt}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NInt(&mut self, _ctx: &NIntContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NInt}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NInt(&mut self, _ctx: &NIntContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NStr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NStr(&mut self, _ctx: &NStrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NStr}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NStr(&mut self, _ctx: &NStrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NBool}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NBool(&mut self, _ctx: &NBoolContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NBool}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NBool(&mut self, _ctx: &NBoolContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NClass}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn enter_NClass(&mut self, _ctx: &NClassContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NClass}
 * labeled alternative in {@link LatteParser#newtype}.
 * @param ctx the parse tree
 */
fn exit_NClass(&mut self, _ctx: &NClassContext<'input>) { }
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
 * Enter a parse tree produced by the {@code ELVal}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ELVal(&mut self, _ctx: &ELValContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ELVal}
 * labeled alternative in {@link LatteParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ELVal(&mut self, _ctx: &ELValContext<'input>) { }
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
 * Enter a parse tree produced by {@link LatteParser#args}.
 * @param ctx the parse tree
 */
fn enter_args(&mut self, _ctx: &ArgsContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#args}.
 * @param ctx the parse tree
 */
fn exit_args(&mut self, _ctx: &ArgsContext<'input>) { }
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
 * Enter a parse tree produced by {@link LatteParser#unOp}.
 * @param ctx the parse tree
 */
fn enter_unOp(&mut self, _ctx: &UnOpContext<'input>) { }
/**
 * Exit a parse tree produced by {@link LatteParser#unOp}.
 * @param ctx the parse tree
 */
fn exit_unOp(&mut self, _ctx: &UnOpContext<'input>) { }
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


