declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L6:
                br label %L0
L1:
                %v7 = phi i32 [%v9, %L0]
                %v8 = sub i32 %v7, 2
                br label %L0
L0:
                %v9 = phi i32 [%v8, %L1], [17, %L6]
                %v10 = icmp sgt i32 %v9, 0
                br i1 %v10, label %L1, label %L2
L2:
                %v11 = phi i32 [%v9, %L0]
                %v12 = icmp slt i32 %v11, 0
                br i1 %v12, label %L3, label %L4
L4:
                call void @printInt (i32 1)
                ret i32 0
L3:
                call void @printInt (i32 0)
                ret i32 0
}