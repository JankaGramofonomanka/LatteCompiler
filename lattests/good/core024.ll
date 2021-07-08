declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s1 = private constant [5 x i8] c"NOOO\00"
@s0 = private constant [4 x i8] c"yes\00"

define i32 @main() {
L4:
                call void @f (i32 1, i32 2)
                ret i32 0
}
define void @f(i32 %v11, i32 %v12) {
L5:
                %v13 = icmp sgt i32 %v12, %v11
                br i1 %v13, label %L0, label %L3
L3:
                %v14 = call i1 @e()
                br i1 %v14, label %L0, label %L1
L1:
                br label %L2
L0:
                %v15 = bitcast [4 x i8]* @s0 to i8*
                call void @printString (i8* %v15)
                br label %L2
L2:
                ret void
}
define i1 @e() {
L6:
                %v16 = bitcast [5 x i8]* @s1 to i8*
                call void @printString (i8* %v16)
                ret i1 0
}