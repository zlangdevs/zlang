; ModuleID = 'zlang_module'
source_filename = "zlang_module"

@str = private unnamed_addr constant [18 x i8] c"Hello from helper\00", align 1
@str.1 = private unnamed_addr constant [14 x i8] c"Starting main\00", align 1
@str.2 = private unnamed_addr constant [14 x i8] c"Called helper\00", align 1

declare i32 @printf(ptr, ...)

declare i32 @puts(ptr)

define void @helper() {
entry:
  %printf = call i32 (ptr, ...) @printf(ptr @str)
  ret void
}

define i32 @main() {
entry:
  %printf = call i32 (ptr, ...) @printf(ptr @str.1)
  call void @helper()
  %printf1 = call i32 (ptr, ...) @printf(ptr @str.2)
  ret i32 0
}
