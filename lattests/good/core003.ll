declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @f() {
L0:
                ret i32 0
}
define i32 @g() {
L1:
                ret i32 0
}
define void @p() {
L2:
                ret void
}
define i32 @main() {
L3:
                call void @p ()
                ret i32 0
}