;; BEGIN HEADER

.class public scopes_reuse_name
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

	invokestatic scopes_reuse_name/main()I
	pop
	return

.end method

;; END HEADER
