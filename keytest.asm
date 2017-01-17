	processor 6502

	org $0100

rc0:	lda $d011
	beq rc0
	lda $d010
	sta $000f
	inc $000e
	jmp rc0

