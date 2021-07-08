declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L6:
                %v10 = call i32 @f(i32 1, i32 -1)
                call void @printInt (i32 %v10)
                ret i32 0
}
define i32 @f(i32 %v11, i32 %v12) {
L7:
                %v13 = icmp sgt i32 %v11, 0
                br i1 %v13, label %L4, label %L3
L4:
                %v14 = phi i32 [%v11, %L7]
                %v15 = phi i32 [%v12, %L7]
                %v16 = icmp sgt i32 %v15, 0
                br i1 %v16, label %L0, label %L3
L3:
                %v17 = phi i32 [%v14, %L4], [%v11, %L7]
                %v18 = phi i32 [%v15, %L4], [%v12, %L7]
                %v19 = icmp slt i32 %v17, 0
                br i1 %v19, label %L5, label %L1
L5:
                %v20 = phi i32 [%v18, %L3]
                %v21 = icmp slt i32 %v20, 0
                br i1 %v21, label %L0, label %L1
L1:
                ret i32 42
L0:
                ret i32 7
}