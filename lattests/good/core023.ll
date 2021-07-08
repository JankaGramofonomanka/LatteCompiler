declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L0:
                %v49 = call i32 @foo(i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2)
                ret i32 %v49
}
define i32 @foo(i32 %v56, i32 %v57, i32 %v58, i32 %v59, i32 %v60, i32 %v61, i32 %v62, i32 %v63, i32 %v50, i32 %v51, i32 %v52, i32 %v53, i32 %v54, i32 %v55) {
L1:
                %v64 = mul i32 2, %v56
                %v65 = sdiv i32 %v57, 2
                %v66 = add i32 %v64, %v65
                %v67 = add i32 %v66, %v58
                %v68 = add i32 %v67, %v59
                %v69 = add i32 %v68, %v60
                %v70 = add i32 %v69, %v61
                %v71 = add i32 %v70, %v62
                %v72 = add i32 %v71, %v63
                %v73 = add i32 %v72, %v50
                %v74 = sdiv i32 %v51, 2
                %v75 = add i32 %v73, %v74
                %v76 = add i32 %v75, %v52
                %v77 = add i32 %v76, %v53
                %v78 = add i32 %v77, %v54
                %v79 = add i32 %v78, %v55
                %v80 = urem i32 %v79, 10
                call void @printInt (i32 %v80)
                ret i32 %v80
}