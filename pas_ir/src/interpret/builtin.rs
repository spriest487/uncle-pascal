use super::MemCell;

pub(super) fn write_ln(args: &[MemCell]) -> Option<MemCell> {
    assert_eq!(1, args.len(), "writeln expected 1 argument");
    match &args[0] {
        MemCell::I32(int) => {
            println!("{}", int);
            None
        }
        _ => panic!("write_ln expected i32 argument")
    }
}