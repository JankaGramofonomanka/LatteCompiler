declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s2 = private constant [2 x i8] c"!\00"
@s0 = private constant [3 x i8] c"&&\00"
@s3 = private constant [6 x i8] c"false\00"
@s4 = private constant [5 x i8] c"true\00"
@s1 = private constant [3 x i8] c"||\00"

define i32 @main() {
L3:
                %v50 = bitcast [3 x i8]* @s0 to i8*
                call void @printString (i8* %v50)
                %v51 = call i1 @test(i32 -1)
                %v52 = call i1 @test(i32 0)
                %v53 = and i1 %v51, %v52
                call void @printBool (i1 %v53)
                %v54 = call i1 @test(i32 -2)
                %v55 = call i1 @test(i32 1)
                %v56 = and i1 %v54, %v55
                call void @printBool (i1 %v56)
                %v57 = call i1 @test(i32 3)
                %v58 = call i1 @test(i32 -5)
                %v59 = and i1 %v57, %v58
                call void @printBool (i1 %v59)
                %v60 = call i1 @test(i32 234234)
                %v61 = call i1 @test(i32 21321)
                %v62 = and i1 %v60, %v61
                call void @printBool (i1 %v62)
                %v63 = bitcast [3 x i8]* @s1 to i8*
                call void @printString (i8* %v63)
                %v64 = call i1 @test(i32 -1)
                %v65 = call i1 @test(i32 0)
                %v66 = or i1 %v64, %v65
                call void @printBool (i1 %v66)
                %v67 = call i1 @test(i32 -2)
                %v68 = call i1 @test(i32 1)
                %v69 = or i1 %v67, %v68
                call void @printBool (i1 %v69)
                %v70 = call i1 @test(i32 3)
                %v71 = call i1 @test(i32 -5)
                %v72 = or i1 %v70, %v71
                call void @printBool (i1 %v72)
                %v73 = call i1 @test(i32 234234)
                %v74 = call i1 @test(i32 21321)
                %v75 = or i1 %v73, %v74
                call void @printBool (i1 %v75)
                %v76 = bitcast [2 x i8]* @s2 to i8*
                call void @printString (i8* %v76)
                call void @printBool (i1 1)
                call void @printBool (i1 0)
                ret i32 0
}
define void @printBool(i1 %v77) {
L4:
                br i1 %v77, label %L1, label %L0
L1:
                %v78 = bitcast [5 x i8]* @s4 to i8*
                call void @printString (i8* %v78)
                br label %L2
L0:
                %v79 = bitcast [6 x i8]* @s3 to i8*
                call void @printString (i8* %v79)
                br label %L2
L2:
                ret void
}
define i1 @test(i32 %v80) {
L5:
                call void @printInt (i32 %v80)
                %v81 = icmp sgt i32 %v80, 0
                ret i1 %v81
}