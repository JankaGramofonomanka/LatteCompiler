declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L3:
                %v8 = call i32 @fac(i32 5)
                call void @printInt (i32 %v8)
                ret i32 0
}
define i32 @fac(i32 %v9) {
L4:
                br label %L0
L1:
                %v10 = phi i32 [%v14, %L0]
                %v11 = phi i32 [%v15, %L0]
                %v12 = mul i32 %v10, %v11
                %v13 = sub i32 %v11, 1
                br label %L0
L0:
                %v14 = phi i32 [%v12, %L1], [1, %L4]
                %v15 = phi i32 [%v13, %L1], [%v9, %L4]
                %v16 = icmp sgt i32 %v15, 0
                br i1 %v16, label %L1, label %L2
L2:
                %v17 = phi i32 [%v14, %L0]
                ret i32 %v17
}