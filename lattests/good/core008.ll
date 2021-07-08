declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L0:
                call void @printInt (i32 -1234234)
                call void @printInt (i32 7)
                ret i32 0
}