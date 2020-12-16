use std::{fmt, ptr::NonNull};

/// The value type of the virtual machine
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    /// Numeric value: `12300`, `-1.23004`, etc
    Number(f64),
    /// `nil`
    Nil,
    /// `true` or `false`
    Bool(bool),
    /// Any heap allocated value
    Object(Object),
}

impl Value {
    /// Returns `true` if this `Value` is `false` or `false` equivalent.
    pub fn is_falsey(&self) -> bool {
        // return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
        match self {
            Value::Nil => true,
            Value::Bool(b) => !*b,
            _ => false,
        }
    }
}

impl From<f64> for Value {
    fn from(src: f64) -> Self {
        Value::Number(src)
    }
}

impl From<bool> for Value {
    fn from(src: bool) -> Self {
        Value::Bool(src)
    }
}

impl From<Object> for Value {
    fn from(src: Object) -> Self {
        Value::Object(src)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => <f64 as fmt::Display>::fmt(n, f),
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => <bool as fmt::Display>::fmt(b, f),
            Value::Object(o) => o.display_fmt(f),
        }
    }
}

/// A memory region that contains `Value`s, separate from the stack.
#[derive(Debug)]
pub struct Heap(bumpalo::Bump);

impl Heap {
    /// Create a new, empty `Heap`.
    pub fn new() -> Self {
        Heap(bumpalo::Bump::new())
    }

    /// Reset the contents of the `Heap`, all `Objects` will be invalid after
    /// calling this.
    pub fn clear(&mut self) {
        self.0.reset()
    }

    /// Allocate a new `StringObject` in the `Heap`.
    pub fn allocate_string(&self, s: impl Into<String>) -> Object {
        let object = self.0.alloc(StringObject {
            base: ObjectBase {
                obj_type: ObjectType::String,
            },
            value: s.into().into_boxed_str(),
        });

        Object {
            ptr: NonNull::new((object as *mut StringObject).cast::<ObjectBase>()).unwrap(),
        }
    }
}

impl Default for Heap {
    fn default() -> Self {
        Heap::new()
    }
}

/// An opaque object reference.
#[derive(Debug, Copy, Clone)]
pub struct Object {
    ptr: NonNull<ObjectBase>,
}

impl Object {
    /// Returns some reference to the value if it is of type T, or None if it
    /// isn't.
    pub fn read<T: ConcreteObject>(&self) -> Option<&T> {
        unsafe { self.ptr.as_ref().downcast_ref::<T>() }
    }

    /// Returns some mutable reference to the value if it is of type T, or None
    /// if it isn't.
    pub fn read_mut<T: ConcreteObject>(&mut self) -> Option<&mut T> {
        unsafe { self.ptr.as_mut().downcast_mut::<T>() }
    }

    /// Write to the formatter the representation that would normally be
    /// encapsulated in the `fmt::Display` trait.
    pub fn display_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let self_base = unsafe { self.ptr.as_ref() };

        match self_base.obj_type {
            ObjectType::String => <StringObject as fmt::Display>::fmt(
                self_base.downcast_ref::<StringObject>().unwrap(),
                f,
            ),
        }
    }

    /// Return `true` if this Object is equivalent to the given Object.
    pub fn partial_eq(&self, other: &Self) -> bool {
        let self_base = unsafe { self.ptr.as_ref() };
        let other_base = unsafe { other.ptr.as_ref() };

        match (self_base.obj_type, other_base.obj_type) {
            (ObjectType::String, ObjectType::String) => <StringObject as PartialEq>::eq(
                self_base.downcast_ref::<StringObject>().unwrap(),
                other_base.downcast_ref::<StringObject>().unwrap(),
            ),
        }
    }

    /// Return `true` if this Object is a reference to the `T` supertype.
    pub fn is<T: ConcreteObject>(&self) -> bool {
        unsafe { self.ptr.as_ref() }.obj_type == T::TYPE
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.partial_eq(other)
    }
}

/// This trait encompasses the behavior of all concrete object types, i.e. all
/// Objects that do not require more casting to be functional.
pub trait ConcreteObject: fmt::Display + PartialEq {
    /// The `ObjectType` of the `ConcreteObject`.
    const TYPE: ObjectType;
}

/// The type of an Object
#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum ObjectType {
    /// The type of an Object that contains immutable text data
    String,
}

/// The base fields of all Objects
#[derive(Debug, Clone, PartialEq, Hash)]
#[repr(C)]
pub struct ObjectBase {
    /// The type of Object that this base is a part of
    pub obj_type: ObjectType,
}

impl ObjectBase {
    fn downcast_ref<T: ConcreteObject>(&self) -> Option<&T> {
        if self.obj_type == T::TYPE {
            Some(unsafe { (self as *const Self).cast::<T>().as_ref().unwrap() })
        } else {
            None
        }
    }

    fn downcast_mut<T: ConcreteObject>(&mut self) -> Option<&mut T> {
        if self.obj_type == T::TYPE {
            Some(unsafe { (self as *mut Self).cast::<T>().as_mut().unwrap() })
        } else {
            None
        }
    }
}

/// An immutable String object
#[derive(Debug, Clone, Hash)]
#[repr(C)]
pub struct StringObject {
    /// The base fields of the `StringObject`.
    pub base: ObjectBase,
    /// The string content
    pub value: Box<str>,
}

impl ConcreteObject for StringObject {
    const TYPE: ObjectType = ObjectType::String;
}

impl fmt::Display for StringObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Display>::fmt(&*self.value, f)
    }
}

impl PartialEq for StringObject {
    fn eq(&self, other: &Self) -> bool {
        (*self.value).eq(&*other.value)
    }
}

impl From<String> for StringObject {
    fn from(src: String) -> Self {
        StringObject {
            base: ObjectBase {
                obj_type: ObjectType::String,
            },
            value: src.into_boxed_str(),
        }
    }
}
