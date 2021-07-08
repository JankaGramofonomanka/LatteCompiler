declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s0 = private constant [4 x i8] c"foo\00"

define i32 @main() {
L6:
                call void @printInt (i32 1)
                call void @printInt (i32 78)
                br label %L0
L1:
                %v18 = phi i32 [%v22, %L0]
                %v19 = phi i32 [%v23, %L0]
                %v20 = sub i32 %v18, 1
                call void @printInt (i32 %v20)
                %v21 = add i32 %v19, 7
                call void @printInt (i32 %v21)
                br label %L0
L0:
                %v22 = phi i32 [%v20, %L1], [78, %L6]
                %v23 = phi i32 [%v21, %L1], [%v17, %L6]
                %v24 = icmp sgt i32 %v22, 76
                br i1 %v24, label %L1, label %L2
L2:
                %v25 = phi i32 [%v22, %L0]
                call void @printInt (i32 %v25)
                %v26 = icmp sgt i32 %v25, 4
                br i1 %v26, label %L3, label %L4
L4:
                %v27 = phi i32 [%v25, %L2]
                %v28 = bitcast [4 x i8]* @s0 to i8*
                call void @printString (i8* %v28)
                br label %L5
L3:
                %v29 = phi i32 [%v25, %L2]
                call void @printInt (i32 4)
                br label %L5
L5:
                %v30 = phi i32 [%v29, %L3], [%v27, %L4]
                call void @printInt (i32 %v30)
                ret i32 0
}