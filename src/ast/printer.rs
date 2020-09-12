//! An implementation of a `Visitor` which prints the structure of the AST

use super::visit::{Visitable, Visitor};

/// The AST printer
#[derive(Debug)]
pub struct Printer;

impl Visitor for Printer {
    type Output = String;

    fn default_output(&self) -> Self::Output {
        String::new()
    }

    fn combine_output(&self, mut o1: Self::Output, o2: Self::Output) -> Self::Output {
        o1.push_str(&o2);

        o1
    }

    fn visit_binary_expr(&mut self, d: &super::BinaryExpr) -> Self::Output {
        let super::BinaryExpr {
            left,
            right,
            operator,
        } = d;

        let o1 = left.visit_with(self);
        let o2 = right.visit_with(self);

        format!("({} {} {})", operator, o1, o2)
    }

    fn visit_grouping_expr(&mut self, d: &super::GroupingExpr) -> Self::Output {
        let super::GroupingExpr { inner } = d;

        let o = inner.visit_with(self);

        format!("(group {})", o)
    }

    fn visit_literal_expr(&mut self, d: &super::LiteralExpr) -> Self::Output {
        let super::LiteralExpr { value } = d;

        value.to_string()
    }

    fn visit_unary_expr(&mut self, d: &super::UnaryExpr) -> Self::Output {
        let super::UnaryExpr { operator, right } = d;

        let o = right.visit_with(self);

        format!("({} {})", operator, o)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{printer::Printer, visit::Visitable, *},
        span::Span,
    };
    use std::sync::Arc;

    #[test]
    fn print_simple_ast() {
        let ast = Expr {
            span: Span::dummy(),
            kind: ExprKind::Binary(BinaryExpr {
                left: Arc::new(Expr {
                    span: Span::dummy(),
                    kind: ExprKind::Unary(UnaryExpr {
                        operator: UnaryOpKind::Negate,
                        right: Arc::new(Expr {
                            span: Span::dummy(),
                            kind: ExprKind::Literal(LiteralExpr {
                                value: crate::scanner::Literal::Number(123.0),
                            }),
                        }),
                    }),
                }),
                operator: BinaryOpKind::Mult,
                right: Arc::new(Expr {
                    span: Span::dummy(),
                    kind: ExprKind::Grouping(GroupingExpr {
                        inner: Arc::new(Expr {
                            span: Span::dummy(),
                            kind: ExprKind::Literal(LiteralExpr {
                                value: crate::scanner::Literal::Number(45.67),
                            }),
                        }),
                    }),
                }),
            }),
        };

        let mut printer = Printer;
        let out = ast.visit_with(&mut printer);

        assert_eq!(out, "(* (- 123) (group 45.67))")
    }
}
