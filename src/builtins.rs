#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinFunction {
    Print,
    Len,
    Range,
    Exception,
    StopIteration,
}

impl BuiltinFunction {
    pub fn name(self) -> &'static str {
        match self {
            Self::Print => "print",
            Self::Len => "len",
            Self::Range => "range",
            Self::Exception => "Exception",
            Self::StopIteration => "StopIteration",
        }
    }

    pub fn callable_id(self) -> u32 {
        match self {
            Self::Print => 1,
            Self::Len => 2,
            Self::Range => 3,
            Self::Exception => 4,
            Self::StopIteration => 5,
        }
    }
}
