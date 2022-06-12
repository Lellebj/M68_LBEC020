	macro push  
	move.l \1,-(sp)
	endm

	macro pop  
	move.l (sp)+,\1
	endm

	macro push_all  
	moveM.L d0-d7/a0-a7,-(sp)
	endm