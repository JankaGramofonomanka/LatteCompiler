declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
L0:
                %v9 = call i32 @readInt()
                %v10 = call i8* @readString()
                %v11 = call i8* @readString()
                %v12 = sub i32 %v9, 5
                call void @printInt (i32 %v12)
                %v13 = add i8* %v10, %v11
                call void @printString (i8* %v13)
                ret i32 0
}