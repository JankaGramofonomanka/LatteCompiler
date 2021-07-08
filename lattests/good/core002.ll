declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s0 = private constant [4 x i8] c"foo\00"

define i32 @main() {
L0:
                call void @foo ()
                ret i32 0
}
define void @foo() {
L1:
                %v5 = bitcast [4 x i8]* @s0 to i8*
                call void @printString (i8* %v5)
                ret void
}