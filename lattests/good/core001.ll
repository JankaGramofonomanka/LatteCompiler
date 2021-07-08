declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
@s0 = private constant [1 x i8] c"\00"
@s3 = private constant [9 x i8] c"/* world\00"
@s1 = private constant [2 x i8] c"=\00"
@s2 = private constant [9 x i8] c"hello */\00"

define i32 @main() {
L24:
                %v59 = call i32 @fac(i32 10)
                call void @printInt (i32 %v59)
                %v60 = call i32 @rfac(i32 10)
                call void @printInt (i32 %v60)
                %v61 = call i32 @mfac(i32 10)
                call void @printInt (i32 %v61)
                %v62 = call i32 @ifac(i32 10)
                call void @printInt (i32 %v62)
                %v63 = bitcast [1 x i8]* @s0 to i8*
                br label %L0
L1:
                %v64 = phi i32 [%v68, %L0]
                %v65 = phi i32 [%v69, %L0]
                %v66 = mul i32 %v65, %v64
                %v67 = sub i32 %v64, 1
                br label %L0
L0:
                %v68 = phi i32 [%v67, %L1], [10, %L24]
                %v69 = phi i32 [%v66, %L1], [1, %L24]
                %v70 = icmp sgt i32 %v68, 0
                br i1 %v70, label %L1, label %L2
L2:
                %v71 = phi i32 [%v69, %L0]
                call void @printInt (i32 %v71)
                %v72 = bitcast [2 x i8]* @s1 to i8*
                %v73 = call i8* @repStr(i8* %v72, i32 60)
                call void @printString (i8* %v73)
                %v74 = bitcast [9 x i8]* @s2 to i8*
                call void @printString (i8* %v74)
                %v75 = bitcast [9 x i8]* @s3 to i8*
                call void @printString (i8* %v75)
                ret i32 0
}
define i32 @fac(i32 %v76) {
L25:
                br label %L3
L4:
                %v77 = phi i32 [%v81, %L3]
                %v78 = phi i32 [%v82, %L3]
                %v79 = mul i32 %v77, %v78
                %v80 = sub i32 %v78, 1
                br label %L3
L3:
                %v81 = phi i32 [%v79, %L4], [1, %L25]
                %v82 = phi i32 [%v80, %L4], [%v76, %L25]
                %v83 = icmp sgt i32 %v82, 0
                br i1 %v83, label %L4, label %L5
L5:
                %v84 = phi i32 [%v81, %L3]
                ret i32 %v84
}
define i32 @rfac(i32 %v3) {
L26:
                %v86 = icmp eq i32 %v85, 0
                br i1 %v86, label %L6, label %L7
L7:
                %v87 = phi i32 [%v85, %L26]
                %v88 = sub i32 %v87, 1
                %v89 = call i32 @rfac(i32 %v88)
                %v90 = mul i32 %v87, %v89
                ret i32 %v90
L6:
                ret i32 1
}
define i32 @mfac(i32 %v4) {
L27:
                %v92 = icmp eq i32 %v91, 0
                br i1 %v92, label %L9, label %L10
L10:
                %v93 = phi i32 [%v91, %L27]
                %v94 = sub i32 %v93, 1
                %v95 = call i32 @nfac(i32 %v94)
                %v96 = mul i32 %v93, %v95
                ret i32 %v96
L9:
                ret i32 1
}
define i32 @nfac(i32 %v5) {
L28:
                %v98 = icmp ne i32 %v97, 0
                br i1 %v98, label %L12, label %L13
L13:
                ret i32 1
L12:
                %v99 = phi i32 [%v97, %L28]
                %v100 = sub i32 %v99, 1
                %v101 = call i32 @mfac(i32 %v100)
                %v102 = mul i32 %v101, %v99
                ret i32 %v102
}
define i32 @ifac(i32 %v6) {
L29:
                %v104 = call i32 @ifac2f(i32 1, i32 %v103)
                ret i32 %v104
}
define i32 @ifac2f(i32 %v105, i32 %v106) {
L30:
                %v107 = icmp eq i32 %v105, %v106
                br i1 %v107, label %L15, label %L16
L16:
                %v108 = phi i32 [%v105, %L30]
                %v109 = phi i32 [%v106, %L30]
                br label %L17
L15:
                %v110 = phi i32 [%v105, %L30]
                ret i32 %v110
L17:
                %v111 = phi i32 [%v108, %L16]
                %v112 = phi i32 [%v109, %L16]
                %v113 = icmp sgt i32 %v111, %v112
                br i1 %v113, label %L18, label %L19
L19:
                %v114 = phi i32 [%v111, %L17]
                %v115 = phi i32 [%v112, %L17]
                br label %L20
L18:
                ret i32 1
L20:
                %v116 = phi i32 [%v114, %L19]
                %v117 = phi i32 [%v115, %L19]
                %v118 = add i32 %v116, %v117
                %v119 = sdiv i32 %v118, 2
                %v120 = call i32 @ifac2f(i32 %v116, i32 %v119)
                %v121 = add i32 %v119, 1
                %v122 = call i32 @ifac2f(i32 %v121, i32 %v117)
                %v123 = mul i32 %v120, %v122
                ret i32 %v123
}
define i8* @repStr(i8* %v125, i32 %v124) {
L31:
                br label %L21
L22:
                %v126 = phi i32 [%v132, %L21]
                %v127 = phi i32 [%v133, %L21]
                %v128 = phi i8* [%v134, %L21]
                %v129 = phi i8* [%v135, %L21]
                %v130 = add i8* %v128, %v129
                %v131 = add i32 %v127, 1
                br label %L21
L21:
                %v132 = phi i32 [%v126, %L22], [%v124, %L31]
                %v133 = phi i32 [%v131, %L22], [0, %L31]
                %v134 = phi i8* [%v130, %L22], [@s0, %L31]
                %v135 = phi i8* [%v129, %L22], [%v125, %L31]
                %v136 = icmp slt i32 %v133, %v132
                br i1 %v136, label %L22, label %L23
L23:
                %v137 = phi i8* [%v134, %L21]
                ret i8* %v137
}