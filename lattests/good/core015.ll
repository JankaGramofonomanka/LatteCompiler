declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L6:
                %v9 = call i32 @ev(i32 17)
                call void @printInt (i32 %v9)
                ret i32 0
}
define i32 @ev(i32 %v10) {
L7:
                %v11 = icmp sgt i32 %v10, 0
                br i1 %v11, label %L0, label %L1
L1:
                %v12 = phi i32 [%v10, %L7]
                %v13 = icmp slt i32 %v12, 0
                br i1 %v13, label %L3, label %L4
L4:
                ret i32 1
L3:
                ret i32 0
L0:
                %v14 = phi i32 [%v10, %L7]
                %v15 = sub i32 %v14, 2
                %v16 = call i32 @ev(i32 %v15)
                ret i32 %v16
}