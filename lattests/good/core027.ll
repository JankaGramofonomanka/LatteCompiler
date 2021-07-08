declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s0 = private constant [4 x i8] c"bad\00"
@s1 = private constant [5 x i8] c"good\00"

define i32 @main() {
L0:
                %v7 = bitcast [4 x i8]* @s0 to i8*
                call void @f (i8* %v7)
                ret i32 0
}
define void @f(i8* %v2) {
L1:
                %v8 = bitcast [5 x i8]* @s1 to i8*
                call void @printString (i8* %v8)
                ret void
}