#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinFunction {
    Print,
    Len,
}

impl BuiltinFunction {
    pub fn callable_id(self) -> u32 {
        match self {
            Self::Print => 1,
            Self::Len => 2,
        }
    }

    pub fn from_callable_id(callable_id: u32) -> Option<Self> {
        match callable_id {
            1 => Some(Self::Print),
            2 => Some(Self::Len),
            _ => None,
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "print" => Some(Self::Print),
            "len" => Some(Self::Len),
            _ => None,
        }
    }
}
