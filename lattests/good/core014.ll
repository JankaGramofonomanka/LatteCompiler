declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L3:
                call void @printInt (i32 1)
                br label %L0
L1:
                %v8 = phi i32 [%v13, %L0]
                %v9 = phi i32 [%v14, %L0]
                %v10 = phi i32 [%v15, %L0]
                call void @printInt (i32 %v9)
                %v11 = add i32 %v8, %v9
                %v12 = sub i32 %v11, %v8
                br label %L0
L0:
                %v13 = phi i32 [%v12, %L1], [1, %L3]
                %v14 = phi i32 [%v11, %L1], [1, %L3]
                %v15 = phi i32 [%v10, %L1], [5000000, %L3]
                %v16 = icmp slt i32 %v14, %v15
                br i1 %v16, label %L1, label %L2
L2:
                ret i32 0
}