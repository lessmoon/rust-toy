use ascii_tree::Tree::{Leaf, Node};
use ascii_tree::{write_tree, Tree};
use core::ops::{Add, Div, Mul, Sub};
use std::any::type_name;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::marker::PhantomData;
use std::ops::Neg;
use std::rc::Rc;

pub trait Set<Inner>
where
    Inner: Copy,
{
    fn set(&mut self, v: Inner);

    fn set_(&mut self, v: &Inner) {
        self.set(*v);
    }
}

pub trait Trim {
    type Output;
    fn trim(&self) -> Self::Output;
}

pub trait OrderLevel {
    fn level() -> u32;
}

pub trait Draw {
    fn tree(&self) -> Tree;
    fn draw(&self) -> String {
        let mut output = String::new();
        let _ = write_tree(&mut output, &(self.tree()));
        output
    }
}

macro_rules! impl_order_level {
    (
        $level:expr;
        $arg:tt
    ) => {
        impl OrderLevel for $arg {
            fn level() -> u32 { $level }
        }
    };

    (
        $level:expr;
        $arg0:tt,$($args:tt),*
    ) => {
        impl_order_level!{$level; $arg0}
        impl_order_level!{$level; $($args),*}
    };

    (
        $level:expr;
        $($types:tt $(:$($bound0:tt)? $(+$bounds:tt)*)?),*;
         $arg:tt
    ) => {
        impl<$($types),*> OrderLevel for $arg<$($types),*>
        where $($types: $($($bound0)? $(+$bounds)*)?),*{
            fn level() -> u32 { $level }
        }
    };

    (
        $level:expr;
        $($types:tt $(:$($bound0:tt)? $(+$bounds:tt)*)?),*;
        $arg0:tt,$($args:tt),*
    ) => {
        impl_order_level!{$level;$($types $(:$($bound0)? $(+$bounds)*)?),*; $arg0}
        impl_order_level!{$level;$($types $(:$($bound0)? $(+$bounds)*)?),*; $($args),*}
    };
}

impl_order_level! {2000; T; Val, Variable}
impl_order_level! {2000; T, U, V; MapOp}
impl_order_level! {2000; T, U; CastOp}
impl_order_level! {0; T, V; AddOp, SubOp}
impl_order_level! {2; T, V; MultOp, DivOp}
impl_order_level! {T::level(); T: OrderLevel; Wrap}

impl<T: Display> Draw for Val<T> {
    fn tree(&self) -> ascii_tree::Tree {
        Leaf(vec![format!("{}{}", self.0, type_name::<T>())])
    }
}

impl<T: Display> Draw for Variable<T> {
    fn tree(&self) -> ascii_tree::Tree {
        Leaf(vec![format!("{}: {}{}", self.name, *self.value.borrow(), type_name::<T>())])
    }
}

impl<T: Draw> Draw for Wrap<T> {
    fn tree(&self) -> ascii_tree::Tree {
        self.0.tree()
    }
}

impl<U, V, A: Draw> Draw for MapOp<U, V, A> {
    fn tree(&self) -> ascii_tree::Tree {
        Node(format!("{}", self.name), vec![self.arg.tree()])
    }
}

impl<U: Draw, V> Draw for CastOp<U, V> {
    fn tree(&self) -> ascii_tree::Tree {
        Node(format!("{}", type_name::<V>()), vec![self.expr.tree()])
    }
}

impl<U: Draw, V: Draw> Draw for AddOp<U, V> {
    fn tree(&self) -> ascii_tree::Tree {
        Node("+".to_string(), vec![self.left.tree(), self.right.tree()])
    }
}

impl<U: Draw, V: Draw> Draw for SubOp<U, V> {
    fn tree(&self) -> ascii_tree::Tree {
        Node("-".to_string(), vec![self.left.tree(), self.right.tree()])
    }
}
impl<U: Draw, V: Draw> Draw for MultOp<U, V> {
    fn tree(&self) -> ascii_tree::Tree {
        Node("Mult".to_string(), vec![self.left.tree(), self.right.tree()])
    }
}
impl<U: Draw, V: Draw> Draw for DivOp<U, V> {
    fn tree(&self) -> ascii_tree::Tree {
        Node("Div".to_string(), vec![self.left.tree(), self.right.tree()])
    }
}

macro_rules! declare_func {
    ($name:tt; $op:tt) => {
        fn $name<U: Expr>(self, other: U) -> $op<Self, U>
        where
            Self: Sized,
        {
            $op::new(self, other)
        }
    };
}

#[derive(Debug, Copy, Clone)]
pub struct Val<T>(T);

impl<T: Copy> Trim for Val<T> {
    type Output = Self;
    fn trim(&self) -> <Self as Trim>::Output {
        *self
    }
}

pub trait Expr {
    type Output;
    fn eval(&self) -> Val<Self::Output>;
    fn w(&self) -> Wrap<Self>
    where
        Self: Sized + Clone,
    {
        Wrap((*self).clone())
    }
    declare_func! {add_; AddOp}
    declare_func! {sub_; SubOp}
    declare_func! {mult_; MultOp}
    declare_func! {div_; DivOp}
    fn as_<T>(self) -> CastOp<Self, T>
    where
        Self: Sized,
    {
        CastOp::<Self, T>::new(self)
    }
    fn map_<T>(self, name: &str, mapper: fn(Self::Output) -> T) -> MapOp<Self::Output, T, Self>
    where
        Self: Sized,
    {
        MapOp::<Self::Output, T, Self>::new(mapper, name, self)
    }
    fn neg_(self) -> MapOp<Self::Output, <Self::Output as Neg>::Output, Self>
    where
        Self::Output: Neg,
        Self: Sized,
    {
        self.map_("-", |v| -v)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CastOp<U, V> {
    pub expr: U,
    _phantom: PhantomData<V>,
}

impl<U, V> Trim for CastOp<U, V>
where
    U: Trim,
{
    type Output = CastOp<<U as Trim>::Output, V>;
    fn trim(&self) -> <Self as Trim>::Output {
        CastOp::<<U as Trim>::Output, V>::new(self.expr.trim())
    }
}

impl<U, V> CastOp<U, V> {
    fn new(expr: U) -> Self {
        Self {
            expr: expr,
            _phantom: PhantomData::<V> {},
        }
    }
}

#[derive(Debug, Clone)]
pub struct MapOp<U, V, A> {
    mapper: fn(U) -> V,
    name: String,
    arg: A,
}

impl<U, V, A> MapOp<U, V, A> {
    fn new(mapper: fn(U) -> V, name: &str, arg: A) -> Self {
        Self {
            mapper: mapper,
            name: String::from(name),
            arg: arg,
        }
    }
}

impl<U, V, A> Expr for MapOp<U, V, A>
where
    A: Expr<Output = U>,
{
    type Output = V;
    fn eval(&self) -> Val<Self::Output> {
        Val((self.mapper)(self.arg.eval().0))
    }
}

impl<U, V, A> Trim for MapOp<U, V, A>
where
    A: Trim,
{
    type Output = MapOp<U, V, <A as Trim>::Output>;
    fn trim(&self) -> <Self as Trim>::Output {
        <Self as Trim>::Output::new(self.mapper, &self.name, self.arg.trim())
    }
}

impl<U, V> Expr for CastOp<U, V>
where
    U: Expr,
    V: From<U::Output>,
{
    type Output = V;

    fn eval(&self) -> Val<Self::Output> {
        Val(V::from(self.expr.eval().0))
    }
}

impl<T> Expr for Val<T>
where
    T: Copy,
{
    type Output = T;

    fn eval(&self) -> Val<Self::Output> {
        *self
    }
}

#[derive(Debug, Clone)]
struct Variable<U> {
    value: Rc<RefCell<U>>,
    name: String,
}

impl<U: Copy> Expr for Variable<U> {
    type Output = U;

    fn eval(&self) -> Val<Self::Output> {
        Val(*(self.value.borrow()))
    }
}

impl<U: Clone> Trim for Variable<U> {
    type Output = Self;

    fn trim(&self) -> <Self as Trim>::Output {
        self.clone()
    }
}

macro_rules! binary_op_gen {
    ($name:tt; $int_trait:tt; $real_op:tt) => {
        #[derive(Debug, Copy, Clone)]
        pub struct $name<U, V> {
            left: U,
            right: V,
        }

        impl<U, V> Trim for $name<U, V>
        where
            U: Trim,
            V: Trim,
        {
            type Output = $name<<U as Trim>::Output, <V as Trim>::Output>;
            fn trim(&self) -> <Self as Trim>::Output {
                $name::new(self.left.trim(), self.right.trim())
            }
        }

        impl<U, V> $name<U, V> {
            fn new(left: U, right: V) -> Self {
                Self { left, right }
            }
        }

        impl<U, V> Expr for $name<U, V>
        where
            U: Expr,
            V: Expr,
            <U as Expr>::Output: $int_trait<<V as Expr>::Output>,
        {
            type Output = <<U as Expr>::Output as $int_trait<<V as Expr>::Output>>::Output;

            fn eval(&self) -> Val<Self::Output> {
                Val(self.left.eval().0 $real_op self.right.eval().0)
            }
        }
    };
}

binary_op_gen! {MultOp; Mul; *}
binary_op_gen! {DivOp; Div; /}
binary_op_gen! {SubOp; Sub; -}
binary_op_gen! {AddOp; Add; +}

impl<U> Variable<U> {
    fn new(value: U, name: &str) -> Self {
        Self {
            value: Rc::new(RefCell::new(value)),
            name: String::from(name),
        }
    }
}

impl<U: Copy> Set<U> for Variable<U> {
    fn set(&mut self, other: U) {
        (*self.value.borrow_mut()) = other;
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Wrap<T>(T);

impl<U> Trim for Wrap<U>
where
    U: Trim + Expr,
{
    type Output = <U as Trim>::Output;
    fn trim(&self) -> <Self as Trim>::Output {
        self.0.trim()
    }
}

impl<T: Expr> Expr for Wrap<T> {
    type Output = T::Output;

    fn eval(&self) -> Val<Self::Output> {
        self.0.eval()
    }
}

impl<U, V> Display for CastOp<U, V>
where
    U: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", type_name::<V>(), self.expr)
    }
}

impl<T> Display for Val<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<U, V> Display for AddOp<U, V>
where
    U: Display,
    V: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} + {}", self.left, self.right)
    }
}

impl<U, V> Display for MultOp<U, V>
where
    U: Display + OrderLevel,
    V: Display + OrderLevel,
    Self: OrderLevel,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if U::level() < Self::level() {
            let _ignore = write!(f, "({})", self.left);
        } else {
            let _ignore = write!(f, "{}", self.left);
        }
        let _ignore = write!(f, " * ");
        if V::level() < Self::level() {
            write!(f, "({})", self.right)
        } else {
            write!(f, "{}", self.right)
        }
    }
}

impl<U, V> Display for DivOp<U, V>
where
    U: Display + OrderLevel,
    V: Display + OrderLevel,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if U::level() < Self::level() {
            let _ignore = write!(f, "({})", self.left);
        } else {
            let _ignore = write!(f, "{}", self.left);
        }
        let _ignore = write!(f, " / ");
        if V::level() <= Self::level() {
            write!(f, "({})", self.right)
        } else {
            write!(f, "{}", self.right)
        }
    }
}

impl<U, V> Display for SubOp<U, V>
where
    U: Display + OrderLevel,
    V: Display + OrderLevel,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if U::level() > V::level() || U::level() > Self::level() {
            write!(f, "{} - {}", self.left, self.right)
        } else {
            write!(f, "{} - ({})", self.left, self.right)
        }
    }
}

impl<T: Display + Expr> Display for Wrap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<U> Display for Variable<U>
where
    U: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<U, V, A> Display for MapOp<U, V, A>
where
    A: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.arg)
    }
}

impl<U: Expr, V: Expr> Add<V> for Wrap<U>
where
    <U as Expr>::Output: Add<<V as Expr>::Output>,
{
    type Output = Wrap<AddOp<Self, V>>;

    fn add(self, other: V) -> Self::Output {
        Wrap(AddOp::new(self, other))
    }
}

impl<U: Expr, V: Expr> Mul<V> for Wrap<U>
where
    <U as Expr>::Output: Mul<<V as Expr>::Output>,
{
    type Output = Wrap<MultOp<Self, V>>;

    fn mul(self, other: V) -> Self::Output {
        Wrap(MultOp::new(self, other))
    }
}

impl<U: Expr, V: Expr> Div<V> for Wrap<U>
where
    <U as Expr>::Output: Div<<V as Expr>::Output>,
{
    type Output = Wrap<DivOp<Self, V>>;

    fn div(self, other: V) -> Self::Output {
        Wrap(DivOp::new(self, other))
    }
}

impl<U: Expr, V: Expr> Sub<V> for Wrap<U>
where
    <U as Expr>::Output: Sub<<V as Expr>::Output>,
{
    type Output = Wrap<SubOp<Self, V>>;

    fn sub(self, other: V) -> Self::Output {
        Wrap(SubOp::new(self, other))
    }
}

impl<T> Val<T>
where
    T: Copy,
{
    fn w(value: T) -> Wrap<Val<T>> {
        Wrap(Val(value))
    }
}

fn main() {
    let mut var = Variable::new(0.32, "x");
    let mut var_i = Variable::new(0, "iy");
    let x =
        Val::w(2.) + var.w() * var.w() / (var.w() - var.w() / Val::w(2.) + var_i.w().as_::<f64>());
    let y = x.add_(var.w());
    let _z = y.w() + y.w().map_("square", |x| x * x);
    let y_t = y.trim();
    println!("{} = {}", y, y.eval());
    //var.set(0.2);
    println!("{} = {}", y_t, y_t.eval());
    var.set(0.2);
    var_i.set(22);
    println!("{} = {}", y_t, y_t.eval());
    let mut a = Variable::new(3i32, "a");
    let mut b = Variable::new(4i32, "b");
    let square = |d: i32| d * d;
    let sqrt = |d: i32| (d as f32).sqrt().round() as i32;
    let c = (a.w().map_("square", square).w() + b.w().map_("square", square))
        .trim()
        .map_("sqrt", sqrt)
        .neg_()
        .map_("str", |x| format!("{}", x));
    print!("{}", y.draw());
    println!("{} = {}", c, c.eval());
    a.set(30i32);
    b.set(40i32);
    println!("{} = {}", c, c.eval());
}
