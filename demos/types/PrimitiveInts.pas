uses System;

let i8: Int8 := -123;
let u8: Byte := 123;
let i16: Int16 := -10001;
let u16: UInt16 := 10001;
let i32: Integer := -10123001;
let u32: UInt32 := 10123001;
let i64: Int64 := -10012312312301;
let u64: UInt64 := 10012312312301;
let isize: NativeInt := -90123331;
let usize: NativeUInt := 90123331;

WriteLn('i8: ' + Int8ToStr(i8));
WriteLn('u8: ' + UInt8ToStr(u8));

WriteLn('i16: ' + Int16ToStr(i16));
WriteLn('u16: ' + UInt16ToStr(u16));

WriteLn('i32: ' + Int32ToStr(i32));
WriteLn('u32: ' + UInt32ToStr(u32));

WriteLn('i64: ' + Int64ToStr(i64));
WriteLn('u64: ' + UInt64ToStr(u64));

WriteLn('isize: ' + NativeIntToStr(isize));
WriteLn('usize: ' + NativeUIntToStr(usize));