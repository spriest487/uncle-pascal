use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum IntConstant {
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
}

impl fmt::Display for IntConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IntConstant::I32(i) => write!(f, "{}", i),
            IntConstant::I64(i) => write!(f, "{}", i),
            IntConstant::U32(i) => write!(f, "{}", i),
            IntConstant::U64(i) => write!(f, "{}", i),
        }
    }
}

impl IntConstant {
    pub fn is_zero(&self) -> bool {
        match self {
            IntConstant::I32(i) => *i == 0,
            IntConstant::U32(i) => *i == 0,
            IntConstant::I64(i) => *i == 0,
            IntConstant::U64(i) => *i == 0,
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        match self {
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
            /* hex literals always produce unsigned values */
            '$' => {
                let val = u64::from_str_radix(&s[1..], 16).ok()?;
                if val <= u32::max_value() as u64 {
                    Some(IntConstant::U32(val as u32))
                } else {
                    Some(IntConstant::U64(val))
                }
            }
            /* negative numbers produce int32, or uint32 if they're too large */
            '-' => {
                let val: i64 = s.parse().ok()?;
                if val >= i32::min_value() as i64 {
                    Some(IntConstant::I32(val as i32))
                } else {
                    Some(IntConstant::I64(val))
                }
            }
            /* positive numbers produce int32, or i64/u64 if they're too large */
            _ => {
                let val: u64 = s.parse().ok()?;
                if val <= i32::max_value() as u64 {
                    Some(IntConstant::I32(val as i32))
                } else if val <= i64::max_value() as u64 {
                    Some(IntConstant::I64(val as i64))
                } else {
                    Some(IntConstant::U64(val))
                }
            }
        }
    }
}