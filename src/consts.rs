use cast;
use std::{
    collections::HashSet,
    fmt::{self, Write},
    ops::{Add, Sub},
    i32,
};

use node::Identifier;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntConstant {
    Char(u8),
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
}

impl fmt::Display for IntConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IntConstant::Char(c) => write!(f, "#{}", c),
            IntConstant::I32(i) => write!(f, "{}", i),
            IntConstant::I64(i) => write!(f, "{}", i),
            IntConstant::U32(i) => write!(f, "${:X}", i),
            IntConstant::U64(i) => write!(f, "{}", i),
        }
    }
}

impl IntConstant {
    pub fn is_zero(&self) -> bool {
        match self {
            IntConstant::Char(c) => *c == 0,
            IntConstant::I32(i) => *i == 0,
            IntConstant::U32(i) => *i == 0,
            IntConstant::I64(i) => *i == 0,
            IntConstant::U64(i) => *i == 0,
        }
    }

    pub fn parse_str(s: &str) -> Option<Self> {
        let s = s.trim();
        let first_char = s.chars().next()?;

        match first_char {
            '#' => {
                let val: u8 = s[1..].parse().ok()?;
                Some(IntConstant::from(val))
            }

            '$' => {
                let val = u64::from_str_radix(&s[1..], 16).ok()?;

                /* hex literals always produce unsigned values */
                if let Ok(val) = cast::u32(val) {
                    Some(IntConstant::U32(val as u32))
                } else {
                    Some(IntConstant::U64(val))
                }
            }

            /* negative numbers produce int32, or uint32 if they're too large */
            '-' => {
                let val: i64 = s.parse().ok()?;
                Some(IntConstant::from(val))
            }

            /* positive numbers produce int32, or i64/u64 if they're too large */
            _ => {
                let val: u64 = s.parse().ok()?;
                Some(IntConstant::from(val))
            }
        }
    }

    pub fn as_i128(&self) -> i128 {
        match self {
            IntConstant::Char(i) => i128::from(*i),
            IntConstant::I32(i) => i128::from(*i),
            IntConstant::U32(i) => i128::from(*i),
            IntConstant::I64(i) => i128::from(*i),
            IntConstant::U64(i) => i128::from(*i),
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        match self {
            IntConstant::Char(i) => Some(*i),
            IntConstant::I32(i) => cast::u8(*i).ok(),
            IntConstant::U32(i) => cast::u8(*i).ok(),
            IntConstant::I64(i) => cast::u8(*i).ok(),
            IntConstant::U64(i) => cast::u8(*i).ok(),
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        match self {
            IntConstant::Char(i) => Some(i32::from(*i)),
            IntConstant::I32(i) => Some(*i),
            IntConstant::U32(i) => cast::i32(*i).ok(),
            IntConstant::I64(i) => cast::i32(*i).ok(),
            IntConstant::U64(i) => cast::i32(*i).ok(),
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        match self {
            IntConstant::Char(i) => Some(u32::from(*i)),
            IntConstant::I32(i) => cast::u32(*i).ok(),
            IntConstant::U32(i) => Some(*i),
            IntConstant::I64(i) => cast::u32(*i).ok(),
            IntConstant::U64(i) => cast::u32(*i).ok(),
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            IntConstant::Char(i) => Some(i64::from(*i)),
            IntConstant::I32(i) => Some(i64::from(*i)),
            IntConstant::U32(i) => Some(i64::from(*i)),
            IntConstant::I64(i) => Some(*i),
            IntConstant::U64(i) => cast::i64(*i).ok(),
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self {
            IntConstant::Char(i) => Some(u64::from(*i)),
            IntConstant::I32(i) => Some(*i as u64),
            IntConstant::U32(i) => Some(u64::from(*i)),
            IntConstant::I64(i) => cast::u64(*i).ok(),
            IntConstant::U64(i) => Some(*i),
        }
    }

    #[cfg(target_pointer_width = "64")]
    pub fn as_usize(&self) -> Option<usize> {
        match self {
            IntConstant::Char(i) => Some(*i as usize),
            IntConstant::I32(i) => Some(*i as usize),
            IntConstant::U32(i) => Some(*i as usize),
            IntConstant::I64(i) => cast::usize(*i).ok(),
            IntConstant::U64(i) => Some(*i as usize),
        }
    }

    #[cfg(target_pointer_width = "32")]
    pub fn as_usize(&self) -> Option<usize> {
        match self {
            IntConstant::Char(i) => Some(*i as usize),
            IntConstant::I32(i) => cast::usize(*i).ok(),
            IntConstant::U32(i) => Some(*i as usize),
            IntConstant::I64(i) => cast::usize(*i).ok(),
            IntConstant::U64(i) => cast::usize(*i).ok(),
        }
    }

    #[cfg(target_pointer_width = "64")]
    pub fn as_isize(&self) -> Option<isize> {
        match self {
            IntConstant::Char(i) => Some(*i as isize),
            IntConstant::I32(i) => Some(*i as isize),
            IntConstant::U32(i) => Some(*i as isize),
            IntConstant::I64(i) => Some(*i as isize),
            IntConstant::U64(i) => cast::isize(*i).ok(),
        }
    }

    #[cfg(target_pointer_width = "32")]
    pub fn as_isize(&self) -> Option<isize> {
        match self {
            IntConstant::Char(i) => Some(*i as isize),
            IntConstant::I32(i) => Some(*i as isize),
            IntConstant::U32(i) => cast::isize(*i).ok(),
            IntConstant::I64(i) => cast::isize(*i).ok(),
            IntConstant::U64(i) => cast::isize(*i).ok(),
        }
    }
}

impl From<u8> for IntConstant {
    fn from(val: u8) -> Self {
        IntConstant::Char(val)
    }
}

impl From<i32> for IntConstant {
    fn from(val: i32) -> Self {
        IntConstant::I32(val)
    }
}

impl From<u32> for IntConstant {
    fn from(val: u32) -> Self {
        IntConstant::from(u64::from(val))
    }
}

impl From<usize> for IntConstant {
    fn from(val: usize) -> Self {
        IntConstant::from(val as u64)
    }
}

impl From<isize> for IntConstant {
    fn from(val: isize) -> Self {
        IntConstant::from(val as i64)
    }
}

impl From<i64> for IntConstant {
    fn from(val: i64) -> Self {
        let i32_min = i64::from(i32::min_value());
        let i32_max = i64::from(i32::max_value());

        if val >= i32_min && val <= i32_max {
            IntConstant::I32(val as i32)
        } else {
            IntConstant::I64(val)
        }
    }
}

impl From<u64> for IntConstant {
    fn from(val: u64) -> Self {
        if let Ok(val) = cast::i32(val) {
            IntConstant::I32(val as i32)
        } else if let Ok(val) = cast::i64(val) {
            IntConstant::I64(val as i64)
        } else {
            IntConstant::U64(val)
        }
    }
}

impl From<i128> for IntConstant {
    fn from(val: i128) -> Self {
        if val > i128::from(i64::max_value()) {
            IntConstant::U64(val as u64)
        } else {
            IntConstant::from(val as i64)
        }
    }
}

impl Add for IntConstant {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self::from(self.as_i128().wrapping_add(rhs.as_i128()))
    }
}

impl Sub for IntConstant {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self::from(self.as_i128().wrapping_sub(rhs.as_i128()))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FloatConstant {
    F64(f64),
}

impl FloatConstant {
    pub fn as_f64(self) -> f64 {
        match self {
            FloatConstant::F64(val) => val,
        }
    }

    pub fn parse_str(s: &str) -> Option<Self> {
        let val: f64 = s.parse().ok()?;
        Some(FloatConstant::from(val))
    }
}

impl From<f64> for FloatConstant {
    fn from(val: f64) -> Self {
        FloatConstant::F64(val)
    }
}

impl Sub for FloatConstant {
    type Output = FloatConstant;

    fn sub(self, rhs: FloatConstant) -> FloatConstant {
        FloatConstant::F64(self.as_f64() - rhs.as_f64())
    }
}

impl Add for FloatConstant {
    type Output = FloatConstant;

    fn add(self, rhs: FloatConstant) -> FloatConstant {
        FloatConstant::F64(self.as_f64() + rhs.as_f64())
    }
}

impl fmt::Display for FloatConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FloatConstant::F64(val) => write!(f, "{:E}", val)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumConstant {
    pub ordinal: u64,
    pub enumeration: Identifier,
}

impl EnumConstant {
    pub fn new(ordinal: u64, enumeration: impl Into<Identifier>) -> Self {
        EnumConstant {
            ordinal,
            enumeration: enumeration.into(),
        }
    }
}

impl fmt::Display for EnumConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.enumeration, self.ordinal)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetConstant {
    pub included_values: HashSet<Identifier>,
    pub set: Identifier,
}

impl SetConstant {
    pub fn new(included_values: impl IntoIterator<Item=impl Into<Identifier>>,
               set: impl Into<Identifier>)
               -> Self {
        SetConstant {
            included_values: included_values.into_iter()
                .map(|val| val.into())
                .collect(),
            set: set.into(),
        }
    }
}

impl fmt::Display for SetConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('[')?;

        let mut first = true;
        for val in &self.included_values {
            if !first {
                f.write_str(", ")?;
                first = false;
            }
            write!(f, "{}", val)?;
        }

        f.write_char(']')
    }
}