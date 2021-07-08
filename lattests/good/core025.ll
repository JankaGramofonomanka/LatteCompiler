declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L0:
                %v4 = mul i32 2, -2
                call void @printInt (i32 %v4)
                ret i32 0
}