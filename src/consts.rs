use std::{
    fmt,
    ops::Add,
    ops::Sub,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
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

    pub fn as_u8(&self) -> Option<u8> {
        match self {
            IntConstant::Char(c) => Some(*c),
            IntConstant::I32(i) => if *i >= 0 && *i < 256 { Some(*i as u8) } else { None },
            IntConstant::U32(i) => if *i < 256 { Some(*i as u8) } else { None },
            IntConstant::I64(i) => if *i >= 0 && *i < 256 { Some(*i as u8) } else { None },
            IntConstant::U64(i) => if *i < 256 { Some(*i as u8) } else { None },
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
                if val <= u32::max_value() as u64 {
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

    fn as_i128(&self) -> i128 {
        match self {
            IntConstant::Char(i) => *i as i128,
            IntConstant::I32(i) => *i as i128,
            IntConstant::U32(i) => *i as i128,
            IntConstant::I64(i) => *i as i128,
            IntConstant::U64(i) => *i as i128,
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

impl From<i64> for IntConstant {
    fn from(val: i64) -> Self {
        if val >= i32::min_value() as i64 && val <= i32::max_value() as i64 {
            IntConstant::I32(val as i32)
        } else {
            IntConstant::I64(val)
        }
    }
}

impl From<u64> for IntConstant {
    fn from(val: u64) -> Self {
        if val <= i32::max_value() as u64 {
            IntConstant::I32(val as i32)
        } else if val <= i64::max_value() as u64 {
            IntConstant::I64(val as i64)
        } else {
            IntConstant::U64(val)
        }
    }
}

impl From<i128> for IntConstant {
    fn from(val: i128) -> Self {
        if val > i64::max_value() as i128 {
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