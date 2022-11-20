use antlr_rust::{
    parser::ParserNodeType,
    tree::{ParseTree, ParseTreeVisitor, VisitChildren},
};
use latte::frontend::parser::{
    build_parser,
    latteparser::{
        AddOpContext, ArgContext, DeclContextAttrs, LatteParserContextType,
        MulOpContext, ProgramContext, RelOpContext,
    },
    lattevisitor::LatteVisitor,
    IntervalDisplayer,
};

struct Visitor {
    indent: usize,
    interval_displayer: IntervalDisplayer,
}

impl<'input> ParseTreeVisitor<'input, LatteParserContextType> for Visitor {
    fn visit_terminal(
        &mut self,
        _node: &antlr_rust::tree::TerminalNode<'input, LatteParserContextType>,
    ) {
        for _ in 0..self.indent {
            print!(" ");
        }
        println!(
            "line:{}, col:{}, token: {}",
            _node.symbol.line, _node.symbol.column, _node.symbol.text
        );
    }

    fn visit_children(&mut self, node: &<LatteParserContextType as ParserNodeType>::Type) {
        self.indent += 4;
        node.get_children().for_each(|child| {
            println!("Item: {}", child.get_text());
            self.visit_node(&*child)
        });
        self.indent -= 4;
        let s = self
            .interval_displayer
            .display_interval(node.get_source_interval());
        // println!("{}", s);
    }
}

impl<'a, 'input> LatteVisitor<'input> for Visitor {
    fn visit_program(&mut self, ctx: &ProgramContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_FnDef(&mut self, ctx: &latte::frontend::parser::latteparser::FnDefContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_BaseCls(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::BaseClsContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_DerivCls(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::DerivClsContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_funDef(&mut self, ctx: &latte::frontend::parser::latteparser::FunDefContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_arg(&mut self, ctx: &ArgContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_classBlock(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::ClassBlockContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_Field(&mut self, ctx: &latte::frontend::parser::latteparser::FieldContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Method(&mut self, ctx: &latte::frontend::parser::latteparser::MethodContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_decl(&mut self, ctx: &latte::frontend::parser::latteparser::DeclContext<'input>) {
        let interval = ctx.get_source_interval();
        println!("Source interval: ({}, {})", interval.a, interval.b);
        self.interval_displayer.display_interval(interval);
        ctx.type_();
        self.visit_children(ctx)
    }

    fn visit_block(&mut self, ctx: &latte::frontend::parser::latteparser::BlockContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Empty(&mut self, ctx: &latte::frontend::parser::latteparser::EmptyContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_BlockStmt(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::BlockStmtContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_VarDecl(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::VarDeclContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_Ass(&mut self, ctx: &latte::frontend::parser::latteparser::AssContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Incr(&mut self, ctx: &latte::frontend::parser::latteparser::IncrContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Decr(&mut self, ctx: &latte::frontend::parser::latteparser::DecrContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Ret(&mut self, ctx: &latte::frontend::parser::latteparser::RetContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_VRet(&mut self, ctx: &latte::frontend::parser::latteparser::VRetContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Cond(&mut self, ctx: &latte::frontend::parser::latteparser::CondContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_CondElse(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::CondElseContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_While(&mut self, ctx: &latte::frontend::parser::latteparser::WhileContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_SExp(&mut self, ctx: &latte::frontend::parser::latteparser::SExpContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Str(&mut self, ctx: &latte::frontend::parser::latteparser::StrContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Arr(&mut self, ctx: &latte::frontend::parser::latteparser::ArrContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Bool(&mut self, ctx: &latte::frontend::parser::latteparser::BoolContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Class(&mut self, ctx: &latte::frontend::parser::latteparser::ClassContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Void(&mut self, ctx: &latte::frontend::parser::latteparser::VoidContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_Int(&mut self, ctx: &latte::frontend::parser::latteparser::IntContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_item(&mut self, ctx: &latte::frontend::parser::latteparser::ItemContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EId(&mut self, ctx: &latte::frontend::parser::latteparser::EIdContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EFunCall(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::EFunCallContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_ERelOp(&mut self, ctx: &latte::frontend::parser::latteparser::ERelOpContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_ETrue(&mut self, ctx: &latte::frontend::parser::latteparser::ETrueContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EOr(&mut self, ctx: &latte::frontend::parser::latteparser::EOrContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EInt(&mut self, ctx: &latte::frontend::parser::latteparser::EIntContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EUnOp(&mut self, ctx: &latte::frontend::parser::latteparser::EUnOpContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EStr(&mut self, ctx: &latte::frontend::parser::latteparser::EStrContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EArrSub(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::EArrSubContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_EMulOp(&mut self, ctx: &latte::frontend::parser::latteparser::EMulOpContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EAnd(&mut self, ctx: &latte::frontend::parser::latteparser::EAndContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EParen(&mut self, ctx: &latte::frontend::parser::latteparser::EParenContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EFalse(&mut self, ctx: &latte::frontend::parser::latteparser::EFalseContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EMetCall(
        &mut self,
        ctx: &latte::frontend::parser::latteparser::EMetCallContext<'input>,
    ) {
        self.visit_children(ctx)
    }

    fn visit_EAddOp(&mut self, ctx: &latte::frontend::parser::latteparser::EAddOpContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_EField(&mut self, ctx: &latte::frontend::parser::latteparser::EFieldContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_addOp(&mut self, ctx: &AddOpContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_mulOp(&mut self, ctx: &MulOpContext<'input>) {
        self.visit_children(ctx)
    }

    fn visit_relOp(&mut self, ctx: &RelOpContext<'input>) {
        self.visit_children(ctx)
    }
}

#[derive(Debug)]
struct ParseError;

fn main() -> Result<(), ParseError> {
    let filename = std::env::args().nth(1).expect("Filename arg missing");

    let (mut parser, was_error, interval_displayer) = build_parser(&filename);
    // println!("Before parsing");
    let ast = parser.program().unwrap();
    // println!("After parsing");

    if was_error.get() {
        return Err(ParseError);
    } else {
        // eprintln/!("Parsed successfully!");
    }

    // let mut visitor = Visitor{indent: 0, interval_displayer};
    // visitor.visit_program(&*ast);

    Ok(())
}
