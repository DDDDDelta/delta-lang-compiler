#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    I32,
    I8,
    Fn(Box<FnType>),
    Ptr(Box<PtrType>),
    Void,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnType {
    arg_ty: Vec<Type>,
    ret_ty: Type,
}

impl FnType {
    pub fn new(arg_ty: Vec<Type>, ret_ty: Type) -> Self {
        FnType { arg_ty, ret_ty }
    }

    pub fn param_ty(&self) -> &Vec<Type> {
        &self.arg_ty
    }

    pub fn ret_ty(&self) -> &Type {
        &self.ret_ty
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PtrType {
    pointee: Type,
}

impl PtrType {
    pub fn new(pointee: Type) -> Self {
        PtrType { pointee }
    }

    pub fn pointee(&self) -> &Type {
        &self.pointee
    }
}
