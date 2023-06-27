	.file "/home/koufesser/Lama-devel/src/test1/../../regression/test004.lama"

	.stabs "/home/koufesser/Lama-devel/src/test1/../../regression/test004.lama",100,0,0,.Ltext

	.globl	main

	.data

_init:	.int 0

	.section custom_data,"aw",@progbits

filler:	.fill	0, 4, 1

	.stabs "x:S1",40,0,0,global_x

global_x:	.int	1

	.stabs "y:S1",40,0,0,global_y

global_y:	.int	1

	.stabs "z:S1",40,0,0,global_z

global_z:	.int	1

	.text

.Ltext:

	.stabs "data:t1=r1;0;4294967295;",128,0,0,0

# IMPORT ("Std") / 

# PUBLIC ("main") / 

# EXTERN ("Llowercase") / 

# EXTERN ("Luppercase") / 

# EXTERN ("LtagHash") / 

# EXTERN ("LflatCompare") / 

# EXTERN ("LcompareTags") / 

# EXTERN ("LkindOf") / 

# EXTERN ("Ltime") / 

# EXTERN ("Lrandom") / 

# EXTERN ("LdisableGC") / 

# EXTERN ("LenableGC") / 

# EXTERN ("Ls__Infix_37") / 

# EXTERN ("Ls__Infix_47") / 

# EXTERN ("Ls__Infix_42") / 

# EXTERN ("Ls__Infix_45") / 

# EXTERN ("Ls__Infix_43") / 

# EXTERN ("Ls__Infix_62") / 

# EXTERN ("Ls__Infix_6261") / 

# EXTERN ("Ls__Infix_60") / 

# EXTERN ("Ls__Infix_6061") / 

# EXTERN ("Ls__Infix_3361") / 

# EXTERN ("Ls__Infix_6161") / 

# EXTERN ("Ls__Infix_3838") / 

# EXTERN ("Ls__Infix_3333") / 

# EXTERN ("Ls__Infix_58") / 

# EXTERN ("Li__Infix_4343") / 

# EXTERN ("Lcompare") / 

# EXTERN ("Lwrite") / 

# EXTERN ("Lread") / 

# EXTERN ("Lfailure") / 

# EXTERN ("Lfexists") / 

# EXTERN ("Lfwrite") / 

# EXTERN ("Lfread") / 

# EXTERN ("Lfclose") / 

# EXTERN ("Lfopen") / 

# EXTERN ("Lfprintf") / 

# EXTERN ("Lprintf") / 

# EXTERN ("LmakeString") / 

# EXTERN ("Lsprintf") / 

# EXTERN ("LregexpMatch") / 

# EXTERN ("Lregexp") / 

# EXTERN ("Lsubstring") / 

# EXTERN ("LmatchSubString") / 

# EXTERN ("Lstringcat") / 

# EXTERN ("LreadLine") / 

# EXTERN ("Ltl") / 

# EXTERN ("Lhd") / 

# EXTERN ("Lsnd") / 

# EXTERN ("Lfst") / 

# EXTERN ("Lhash") / 

# EXTERN ("Lclone") / 

# EXTERN ("Llength") / 

# EXTERN ("Lstring") / 

# EXTERN ("LmakeArray") / 

# EXTERN ("LstringInt") / 

# EXTERN ("global_sysargs") / 

# EXTERN ("Lsystem") / 

# EXTERN ("LgetEnv") / 

# EXTERN ("Lassert") / 

# LABEL ("main") / 

main:

# BEGIN ("main", 2, 0, [], [], []) / 

	.type main, @function

	.cfi_startproc

	movl	_init,	%eax
	test	%eax,	%eax
	jz	_continue
	ret
_continue:

	movl	$1,	_init
	pushl	%ebp
	.cfi_def_cfa_offset	8

	.cfi_offset 5, -8

	movl	%esp,	%ebp
	.cfi_def_cfa_register	5

	subl	$Lmain_SIZE,	%esp
	movl	%esp,	%edi
	movl	$filler,	%esi
	movl	$LSmain_SIZE,	%ecx
	rep movsl	
	call	__gc_init
	pushl	12(%ebp)
	pushl	8(%ebp)
	call	set_args
	addl	$8,	%esp
# SLABEL ("L1") / 

L1:

# LINE (3) / 

	.stabn 68,0,3,.L0

.L0:

# CALL ("Lread", 0, false) / 

	call	Lread
	addl	$0,	%esp
	movl	%eax,	%ebx
# LINE (1) / 

	.stabn 68,0,1,.L1

.L1:

# ST (Global ("x")) / 

	movl	%ebx,	global_x
# DROP / 

# LINE (4) / 

	.stabn 68,0,4,.L2

.L2:

# CALL ("Lread", 0, false) / 

	call	Lread
	addl	$0,	%esp
	movl	%eax,	%ebx
# ST (Global ("y")) / 

	movl	%ebx,	global_y
# DROP / 

# SLABEL ("L16") / 

L16:

# SLABEL ("L20") / 

L20:

# LINE (5) / 

	.stabn 68,0,5,.L3

.L3:

# LD (Global ("x")) / 

	movl	global_x,	%ebx
# LD (Global ("y")) / 

	movl	global_y,	%ecx
# BINOP ("+") / 

	addl	%ecx,	%ebx
	decl	%ebx
# SLABEL ("L21") / 

L21:

# SLABEL ("L24") / 

L24:

# LD (Global ("x")) / 

	movl	global_x,	%ecx
# LD (Global ("y")) / 

	movl	global_y,	%esi
# BINOP ("-") / 

	subl	%esi,	%ecx
	orl	$0x0001,	%ecx
# SLABEL ("L25") / 

L25:

# BINOP ("+") / 

	addl	%ecx,	%ebx
	decl	%ebx
# SLABEL ("L17") / 

L17:

# SLABEL ("L28") / 

L28:

# SLABEL ("L32") / 

L32:

# LD (Global ("x")) / 

	movl	global_x,	%ecx
# LD (Global ("y")) / 

	movl	global_y,	%esi
# BINOP ("-") / 

	subl	%esi,	%ecx
	orl	$0x0001,	%ecx
# SLABEL ("L33") / 

L33:

# SLABEL ("L36") / 

L36:

# LD (Global ("x")) / 

	movl	global_x,	%esi
# LD (Global ("y")) / 

	movl	global_y,	%edi
# BINOP ("+") / 

	addl	%edi,	%esi
	decl	%esi
# SLABEL ("L37") / 

L37:

# BINOP ("-") / 

	subl	%esi,	%ecx
	orl	$0x0001,	%ecx
# SLABEL ("L29") / 

L29:

# BINOP ("+") / 

	addl	%ecx,	%ebx
	decl	%ebx
# ST (Global ("z")) / 

	movl	%ebx,	global_z
# DROP / 

# LINE (6) / 

	.stabn 68,0,6,.L4

.L4:

# LD (Global ("z")) / 

	movl	global_z,	%ebx
# CALL ("Lwrite", 1, false) / 

	pushl	%ebx
	call	Lwrite
	addl	$4,	%esp
	movl	%eax,	%ebx
# SLABEL ("L2") / 

L2:

# END / 

	movl	%ebx,	%eax
Lmain_epilogue:

	movl	%ebp,	%esp
	popl	%ebp
	xorl	%eax,	%eax
	.cfi_restore	5

	.cfi_def_cfa	4, 4

	ret
	.cfi_endproc

	.set	Lmain_SIZE,	0

	.set	LSmain_SIZE,	0

	.size main, .-main

