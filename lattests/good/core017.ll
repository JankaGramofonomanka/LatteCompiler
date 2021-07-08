declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s0 = private constant [4 x i8] c"apa\00"
@s2 = private constant [6 x i8] c"false\00"
@s1 = private constant [5 x i8] c"true\00"

define i32 @main() {
L7:
                %v40 = icmp sle i32 3, 4
                br i1 %v40, label %L3, label %L1
L3:
                %v41 = phi i32 [4, %L7]
                br label %L0
L1:
                %v42 = phi i32 [4, %L7]
                %v43 = bitcast [4 x i8]* @s0 to i8*
                call void @printString (i8* %v43)
                br label %L2
L0:
                %v44 = phi i32 [%v41, %L3]
                call void @printBool (i1 1)
                br label %L2
L2:
                %v45 = phi i32 [%v44, %L0], [%v42, %L1]
                %v46 = icmp eq i1 1, 1
                %v47 = call i1 @dontCallMe(i32 1)
                %v48 = or i1 %v46, %v47
                call void @printBool (i1 %v48)
                %v49 = icmp slt i32 4, -5
                %v50 = call i1 @dontCallMe(i32 2)
                %v51 = and i1 %v49, %v50
                call void @printBool (i1 %v51)
                %v52 = icmp eq i32 4, %v45
                %v53 = icmp eq i1 1, 1
                %v54 = and i1 %v53, 1
                %v55 = and i1 %v52, %v54
                call void @printBool (i1 %v55)
                %v56 = call i1 @implies(i1 0, i1 0)
                call void @printBool (i1 %v56)
                %v57 = call i1 @implies(i1 0, i1 1)
                call void @printBool (i1 %v57)
                %v58 = call i1 @implies(i1 1, i1 0)
                call void @printBool (i1 %v58)
                %v59 = call i1 @implies(i1 1, i1 1)
                call void @printBool (i1 %v59)
                ret i32 0
}
define i1 @dontCallMe(i32 %v2) {
L8:
                call void @printInt (i1 %v60)
                ret i1 1
}
define void @printBool(i1 %v61) {
L9:
                br i1 %v61, label %L4, label %L5
L5:
                %v62 = bitcast [6 x i8]* @s2 to i8*
   