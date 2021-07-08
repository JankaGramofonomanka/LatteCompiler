declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L3:
                %v7 = add i32 56, 45
                %v8 = icmp sle i32 %v7, 2
                br i1 %v8, label %L0, label %L1
L1:
                br label %L2
L0:
                br label %L2
L2:
                %v9 = phi i32 [1, %L0], [2, %L1]
                call void @printInt (i32 %v9)
                ret i32 0
}