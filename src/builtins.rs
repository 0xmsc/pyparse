#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinFunction {
    Print,
    Len,
}

impl BuiltinFunction {
    pub fn name(self) -> &'static str {
        match self {
            Self::Print => "print",
            Self::Len => "len",
        }
    }

    pub fn callable_id(self) -> u32 {
        match self {
            Self::Print => 1,
            Self::Len => 2,
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
