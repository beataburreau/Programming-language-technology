;; BEGIN HEADER

.class public fun_return_incorrect_type
.super java/lang/Object

.method public <init>()V
.limit locals 1

	aload_0
	invokespecial java/lang/Object/<init>()V
	return

.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1
.limit stack  1

	invokestatic fun_return_incorrect_type/main()I
	pop
	return

.end method

;; END HEADER
