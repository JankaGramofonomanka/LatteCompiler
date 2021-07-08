declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L0:
                %v4 = sdiv i32 -42, -1
                call void @printInt (i32 %v4)
                ret i32 0
}