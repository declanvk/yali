//! Implementation of the visitor pattern for the AST data structure

use super::{BinaryExpr, Expr, GroupingExpr, LiteralExpr, UnaryExpr};
use std::fmt;

/// A type which traverses `Visitable` data
#[allow(missing_docs)]
pub trait Visitor: Sized {
    /// The type of output produced by this `Visitor`
    type Output;

    /// The default value for this `Visitor`
    fn default_output(&self) -> Self::Output;

    /// Combine the output of 2 instances of visiting some data
    fn combine_output(&self, o1: Self::Output, o2: Self::Output) -> Self::Output;

    /// Combine the output of multiple instances of visiting some data
    fn combine_many_output<IT: IntoIterator<Item = Self::Output>>(&self, it: IT) -> Self::Output {
        let mut iter = it.into_iter();
        if let Some(first) = iter.next() {
            iter.fold(first, |accum, next| self.combine_output(accum, next))
        } else {
            self.default_output()
        }
    }

    fn visit_expr(&mut self, d: &Expr) -> Self::Output {
        d.super_visit_with(self)
    }

    fn visit_binary_expr(&mut self, d: &BinaryExpr) -> Self::Output {
        d.super_visit_with(self)
    }

    fn visit_grouping_expr(&mut self, d: &GroupingExpr) -> Self::Output {
        d.super_visit_with(self)
    }

    fn visit_literal_expr(&mut self, d: &LiteralExpr) -> Self::Output {
        d.super_visit_with(self)
    }

    fn visit_unary_expr(&mut self, d: &UnaryExpr) -> Self::Output {
        d.super_visit_with(self)
    }
}

/// A type which is able to be traversed by a `Visitor`
pub trait Visitable: fmt::Debug {
    /// Perform the default visiting of this piece of data
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output;

    /// Visit the this piece of data with the given `Visitor`
    fn visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        self.super_visit_with(visitor)
    }
}

///////////////////////////////////
////// STRUCTURAL VISIT IMPLS //////
///////////////////////////////////

impl<T: Visitable> Visitable for Vec<T> {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let outputs: Vec<_> = self.iter().map(|ty| ty.visit_with(visitor)).collect();
        visitor.combine_many_output(outputs)
    }
}

impl<T: Visitable> Visitable for std::sync::Arc<T> {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        (**self).visit_with(visitor)
    }
}

impl<T: Visitable> Visitable for Option<T> {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        self.as_ref()
            .map(|inner| inner.visit_with(visitor))
            .unwrap_or_else(|| visitor.default_output())
    }
}

impl<T: Visitable, E: Clone + fmt::Debug> Visitable for Result<T, E> {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        match self {
            Ok(inner) => inner.visit_with(visitor),
            Err(_) => visitor.default_output(),
        }
    }
}

impl<T: Visitable, U: Visitable> Visitable for (T, U) {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        let out1 = self.0.visit_with(visitor);
        let out2 = self.1.visit_with(visitor);
        visitor.combine_output(out1, out2)
    }
}

impl<T: Visitable> Visitable for &T {
    fn super_visit_with<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        <T as Visitable>::visit_with(self, visitor)
    }
}
