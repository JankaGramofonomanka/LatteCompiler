declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s1 = private constant [2 x i8] c" \00"
@s2 = private constant [14 x i8] c"concatenation\00"
@s4 = private constant [6 x i8] c"false\00"
@s0 = private constant [7 x i8] c"string\00"
@s3 = private constant [5 x i8] c"true\00"

define i32 @main() {
L3:
                %v33 = add i32 56, -23
                call void @printInt (i32 %v33)
                %v34 = sub i32 56, -23
                call void @printInt (i32 %v34)
                %v35 = mul i32 56, -23
                call void @printInt (i32 %v35)
                %v36 = sdiv i32 45, 2
                call void @printInt (i32 %v36)
                %v37 = urem i32 78, 3
                call void @printInt (i32 %v37)
                %v38 = sub i32 56, -23
                %v39 = add i32 56, -23
                %v40 = icmp sgt i32 %v38, %v39
                call void @printBool (i1 %v40)
                %v41 = sdiv i32 56, -23
                %v42 = mul i32 56, -23
                %v43 = icmp sle i32 %v41, %v42
                call void @printBool (i1 %v43)
                %v44 = bitcast [7 x i8]* @s0 to i8*
                %v45 = bitcast [2 x i8]* @s1 to i8*
                %v46 = add i8* %v44, %v45
                %v47 = bitcast [14 x i8]* @s2 to i8*
                %v48 = add i8* %v46, %v47
                call void @printString (i8* %v48)
                ret i32 0
}
define void @printBool(i1 %v49) {
L4:
                br i1 %v49, label %L0, label %L1
L1:
                %v50 = bitcast [6 x i8]* @s4 to i8*
                call void @printString (i8* %v50)
                ret void
L0:
                %v51 = bitcast [5 x i8]* @s3 to i8*
                call void @printString (i8* %v51)
                ret void
}