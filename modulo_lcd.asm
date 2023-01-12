;
;			MODULO ASM LCD 4 BITS
;
;*******************************************************************
;o-----o-----o-----o-----o-----o-----o-----o-----o-----o-----o-----o
;*******************************************************************
;
;	Some frequently used code fragments
;	Use macros to make mistreaks consistently.
;
;-------------------------------------------------------------------
;	Select Register Bank 0

bank0	macro
	errorlevel	+302		; Re-enable bank warning
	bcf		STATUS,RP0	; Select Bank 0
	endm

;-------------------------------------------------------------------
;	Select Register Bank 1

bank1	macro
	bsf		STATUS,RP0	; Select Bank 1
	errorlevel	-302		; disable warning
	endm

;-------------------------------------------------------------------
;	Swap bytes in register file via W

swap	macro	this,that

	movf	this,w		; get this
	xorwf	that,f		; Swap using Microchip
	xorwf	that,w		; Tips'n Tricks
	xorwf	that,f		; #18
	movwf	this

	endm
;-------------------------------------------------------------------
;	Copy bytes in register file via W

copy	macro	from,to

	MOVF	from,W
	MOVWF	to

	endm
;*******************************************************************
;
;	CPU configuration
;

	MESSG		"Processor = 16F628"
	processor	16f628
	include		<p16f628.inc>
	__CONFIG        _CP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC & _BODEN_ON & _LVP_OFF

;**********************************************************
;
;	I/O Assignments.
;

#define	LCD0		PORTB,3
#define	LCD1		PORTB,2
#define	LCD2		PORTB,1
#define	LCD3		PORTB,0
#define	ENA		PORTB,4		; Display "E"
#define	RS		PORTB,5		; Display "RS"
#DEFINE TRIS_LCD	TRISB

;*******************************************************************
;
;	file register declarations: uses only registers in bank0
;	bank 0 file registers begin at 0x0c in the 16F84
;	and at 0x20 in the 16F628
;
;*******************************************************************

	cblock	0x20
;
;       Floating Point Stack and other locations used by FP.TXT
;
;	FP Stack: TOS	A = 	AEXP:AARGB0:AARGB1:AARGB3:AARGB4
;			B = 	BEXP:BARGB0:BARGB1:BARGB2
;			C = 	CEXP:CARGB0:CARGB1

	AARGB4
	AARGB3
	AARGB2
	AARGB1
	AARGB0
	AEXP			; 8 bit biased exponent for argument A
	SIGN			; save location for sign in MSB

	FPFLAGS			; floating point library exception flags
	
	BARGB2
	BARGB1
	BARGB0
	BEXP			; 8 bit biased exponent for argument B

	TEMPB3			; 1 Unused byte
	TEMPB2			; 1 Unused byte
	TEMPB1			; Used
	TEMPB0			; 1 Unused byte

	CARGB1
	CARGB0			; most significant byte of argument C
	CEXP			; 8 bit biased exponent for argument C

;
;	"Main" Program Storage
;
 
	COUNT			; Bin to BCD convert (bit count)
	cnt			;                    (BCD BYTES)

	CHR 

	F1:2
	F2:2
	F3:2
	
	bcd:4			; BCD, MSD first 

	TabStop			; Used to fix bad displays.
	TabTemp

	FPE			; Collect FP errors in here
	
	R_sign			; Holds "+" or " " (sign)

	EEflag:1		; Cal adjust flag
	
	endc

	cblock	0x70		; Common RAM

	EE_ADR
	EE_DATA0
	EE_DATA1		

	PB_data:1		; LCD output munger temp
	
	links:1			; User test links copy

	COUNT1			; Used by delay routines
				; and "prescaler flush"
	COUNT2			; Timing (100ms)

	endc

EXP	equ	AEXP		; Used by FP.TXT
TEMP	equ	TEMPB0
;AARG	equ	AARGB0		; Unused
;BARG	equ	BARGB0		; Unused
;CARG	equ	CARGB0		; Unused

;*******************************************************************
;
;       GENERAL MATH LIBRARY DEFINITIONS
;
;
;	define assembler constants

B0		equ	0
B1		equ	1
B2		equ	2
B3		equ	3
B4		equ	4
B5		equ	5
B6		equ	6
B7		equ	7

MSB		equ	7
LSB		equ	0

;     STATUS bit definitions

#define	_C	STATUS,0
#define	_Z	STATUS,2

;*******************************************************************
;
;       FLOATING POINT literal constants
;

EXPBIAS         equ     D'127'

;
;       floating point library exception flags
;

IOV             equ     0       ; bit0 = integer overflow flag

FOV             equ     1       ; bit1 = floating point overflow flag

FUN             equ     2       ; bit2 = floating point underflow flag

FDZ             equ     3       ; bit3 = floating point divide by zero flag

NAN		equ	4	; bit4 = not-a-number exception flag

DOM		equ	5	; bit5 = domain error exception flag

RND             equ     6       ; bit6 = floating point rounding flag, 0 = truncation
                                ; 1 = unbiased rounding to nearest LSB

SAT             equ     7       ; bit7 = floating point saturate flag, 0 = terminate on
                                ; exception without saturation, 1 = terminate on
                                ; exception with saturation to appropriate value

;**********************************************************
;
;	Motorola syntax branches
;

#define	beq	bz 
#define	BEQ	bz
#define	BNE	bnz
#define	bne	bnz

#define	BCC	bnc
#define	bcc	bnc
#define	BCS	bc
#define	bcs	bc

#define	BRA	goto
#define	bra	goto


;**********************************************************
;
;	Begin Executable Stuff(tm)
;

	org	0x00

GO	clrwdt			; 0 << Reset
	call	InitIO

;**********************************************************
;
;	Main Program
;

START	CALL	LCDINIT	 	; INITIALIZE LCD MODULE	


;===================================================================
;
;			FUNÇÕES GERAIS
;
;===================================================================
;********************************************************************
;       Initialise Input & Output devices
;********************************************************************
INIt_IO	movlw	b'00000110'
	movwf	CMCON 	   	; Select Comp mode

	bank1

	movlw	b'00000000'
	movwf	VRCON 	   	; Set Volt ref mode to OFF

	movlw	0x37		; Option register
	movwf	OPTION_REG	; Port B weak pull-up enabled
				; INTDEG Don't care
				; Count RA4/T0CKI
				; Count on falling edge
				; Prescale Timer/counter
				; divide Timer/counter by 256

				; PORTB:-
	movlw	b'01000000'	; initialise data direction
				; 1 = input
				; 0 = output
				;
	movwf	TRIS_LCD	; PORTB<0>   = LCD out "DB4"
				; PORTB<1>   =         "DB5"
				; PORTB<2>   =         "DB6"
				; PORTB<3>   =         "DB7"
				; PORTB<4>   = E  out to LCD
				; PORTB<5>   = RS out to LCD
				; PORTB<6>   = function in
				; PORTB<7>   = Ccal switch out

	bank0

	return	

;=========================================================
;
;		FUNÇÕES PARA O LCD
;
;=========================================================
;*********** INITIALISE LCD MODULE 4 BIT MODE ***********************

LCDINIT CALL	MS100		; WAIT FOR LCD MODULE HARDWARE RESET
	BCF	RS		; REGISTER SELECT LOW
	BCF	ENA		; ENABLE LINE LOW
	
	MOVLW	0x03		; 1
	call	PB_dly
	CALL	MS100		; WAIT FOR DISPLAY TO CATCH UP

	MOVLW	0x03		; 2
	call	PB_dly

	MOVLW	0x03		; 3
	call	PB_dly

	MOVLW	0x02		; Fn set 4 bits
	call	PB_dly
	
	MOVLW	0x0C		; 0x0C DISPLAY ON
	CALL	ST200us

	MOVLW	0x28		; DISPLAY 2 Line , 5x7 Dot's
	CALL	ST200us		; New in LC628/LC006 version
	
	MOVLW	0x06		; 0x06 ENTRY MODE SET
	CALL	ST200us		; Fall into CLEAR

;************ CLEAR DISPLAY ***************************

CLEAR	MOVLW	0x01		; CLEAR DISPLAY
	goto	Home2		; LONGER DELAY NEEDED WHEN CLEARING DISPLAY

;*********** MOVE TO HOME *****************************

HOME	movlw	0x09		; Count characters
	movwf	TabStop		; before tickling display.

	MOVLW	0x02		; HOME DISPLAY
Home2	CALL	CMD
	goto	MS2

;**********************************************************
;
;	SENDS DATA TO LCD DISPLAY MODULE (4 BIT MODE)	
;

CMD	BCF	RS		; SELECT COMMAND REGISTER
	GOTO	SEND

;************ MOVE TO START OF LINE 2 *****************

LINE2	MOVLW	0xC0		; ADDRESS FOR SECOND LINE OF DISPLAY

ST200us	CALL	STROBE
	goto	D200us

;
;	Print String addressed by W
;	Note: Strings are in EEPROM
;	We do a lotta bank switching here.

pmsg	bank1
	CALL	EE_RD		; pointer

	MOVF	EE_DATA1,W

        bank0			; Does not change Z bit
 
	btfsS	STATUS,Z	; ZERO = All done
	P_LCD			; so quit
	
	bank1
	MOVF	EE_DATA0,W

	bank0			; Does not change Z bit
 
	btfsc	STATUS,Z	; ZERO = All done
	return			; so quit

P_LCD	call	DADO		; Byte -> display

	bank1
	INCF    EEADR,F         ; bump address
	goto	pmsg

;**********************************************************
;
;	Put a BCD nybble to display
;
NUN	ANDLW	0x0F		; MASK OFF OTHER PACKED BCD DIGIT
	ADDLW	0x30		; Convert BIN to ASCII

;**********************************************************
;
;	Put a byte to display
;

DADO	decf	TabStop,F	; Time to tickle bad display?
	bne	DATA		; Not yet
	
	movwf	TabTemp		; Save character
	
;	btfss	FIXIT		; Check if we got a crook one.
;	CALL	LINE2		; Skip this if good

	movf	TabTemp,W	; Restore character

DATA	BSF	RS		; SELECT DATA REGISTER
SEND	MOVWF	CHR		; STORE CHAR TO DISPLAY
	SWAPF	CHR,W		; SWAP UPPER AND LOWER NIBBLES (4 BIT MODE)

	call	PB_dly

	MOVF	CHR,W		; GET CHAR AGAIN 

;**********************************************************
;
;	Put 4 bits to LCD & wait (untrimmed)
;

NSEND	movwf	PB_data		; Save nybble

	btfss	PB_data,0	; copy LSbit
	bcf	LCD0
	btfsc	PB_data,0
	bsf	LCD0
	
	btfss	PB_data,1
	bcf	LCD1
	btfsc	PB_data,1
	bsf	LCD1
	
	btfss	PB_data,2
	bcf	LCD2
	btfsc	PB_data,2
	bsf	LCD2
	
	btfss	PB_data,3	; copy MSbit
	bcf	LCD3
	btfsc	PB_data,3
	bsf	LCD3
	
	BSF	ENA		; ENA HIGH
	NOP			
	BCF	ENA		; ENA LOW 

	goto	D200us		; Fall into DELAY subroutine


;**********************************************************
;		DELAYS
;
;**********************************************************
;
;	Delay for 200us (untrimmed)
;

D200us	MOVLW	0x42		; DELAY  200us
	MOVWF	COUNT1	
NXT5	DECFSZ	COUNT1,F
	GOTO	NXT5	
	RETLW	0

;**********************************************************
;
;	Delay for 2ms (untrimmed)
;
;**********************************************************

MS2	MOVLW	0xFD		; DELAY 2ms
	MOVWF	COUNT1

	MOVLW	0x66
	MOVWF	COUNT2

	goto	L3		

;**********************************************************
;
;	Delay for about 200ms or 300ms (untrimmed)
;
;**********************************************************

MS300	call	MS100
MS200	call	MS100

;**********************************************************
;
;	Delay for about 100ms
;
;**********************************************************
MS100	MOVLW	0x7e		; Count up
	MOVWF	COUNT1		; to roll-over

	MOVLW	0x20		; was 0x19, then 0x25, then 1f
	MOVWF	COUNT2			

L3	INCFSZ	COUNT2,F
	GOTO	L3

	INCFSZ	COUNT1,F
	GOTO	L3

	RETLW	0

;**********************************************************
;
;	Display contents of AARGB0,1,2 on LCD
;	First convert to BCD, Then ASCII (nybble at a time)
;

Display	CALL	B2_BCD		; CONVERT COUNT TO BCD		

	call	Swap0		; GET NEXT DIGIT
	call	Move0		; GET OTHER BCD DIGIT
	call	Swap1
	call	Move1
	call	Swap2
	call	Move2
	call	Swap3
	goto	Move3		; includes return

;********************************************************************
;
;	Stack operations
;
;********************************************************************	

;add		call	FPA24
;		goto	S_fix

subtract	call	FPS24
		goto	S_fix

divide		call	FPD24
		goto	S_fix

multiply	call	FPM24
;		goto	S_fix

;
;	Fix stack after add, subtract, divide & multiply
;	AND Collect ALL Floating Point Errors in FPE

S_fix	iorwf	FPE,f			; W may hold Error (0xff)

	copy	CARGB1,BARGB1		; C -> B
	copy	CARGB0,BARGB0
	copy	CEXP,BEXP
	return

;
;	Push stack (duplicates TOS)
;

S_push	copy	BARGB1,CARGB1		; B -> C
	copy	BARGB0,CARGB0
	copy	BEXP,CEXP

	copy	AARGB1,BARGB1		; A -> B
	copy	AARGB0,BARGB0
	copy	AEXP,BEXP
	return

;
;	Swap A and B

S_swap	swap	AARGB1,BARGB1		; A <-> B
	swap	AARGB0,BARGB0
	swap	AEXP,BEXP
	return

;********************************************************************
;	
;	MORE STACK OPERATION
;
;********************************************************************	

Get_Lscale	call	S_push		; make room first
		movlw	0xB8		; 2.53303e+17
		movwf	AEXP		; Create FP version of
		movlw	0x60		; Precomputed 1/(4*PI*PI)
		movwf	AARGB0		; times any needed
		movlw	0xFA		; fiddle factor (1/100)
		movwf	AARGB1
		return

NUN_BCD		call	S_push		; make room first
		clrf	AEXP		; Create a binary 1
		clrf	AARGB0
		clrf	AARGB1
		movlw	'NUN
		goto	LSB2stak
	

NUN_FLOAT	movlw	'FLOAT		; Get integer value
		goto	W2stak		; Includes stack push

;********************************************************************
;	Copy 16 bit number, pointed to by W, to stack
;	and convert to FP (positive value only)
;	via a 24 bit number in AARGB0,1,2
;********************************************************************	

W2stak		movwf	FSR
		call	S_push		; make room first

		clrf	AEXP
		clrf	AARGB0

		movf	INDF,W		; Big Byte first
		movwf	AARGB1

		incf	FSR,F		; then little byte

		movf	INDF,W
LSB2stak	movwf	AARGB2
		
		CALL	FLO2424		; 24 bit int -> 24 bit FP
		iorwf	FPE,f		; W may hold Error (0xff)
		RETURN

;********************************************************************
;	Read EEPROM 
;********************************************************************	

EE_RD	bank1
;	movlw	EE_ADR		; Address to read
	MOVWF	EEADR
	bank0

	CALL	EE_R
	MOVWF	EE_DATA0

	CALL	EE_Rinc
	MOVWF	EE_DATA1

	RETURN

EE_Rinc	bank1
	INCF	EEADR,F		; bump address

EE_R	bank1
	BSF	EECON1,RD	; EE Read
	MOVF	EEDATA,W	; W = EEDATA
	bank0

	RETURN

;********************************************************************
;	Write EEPROM 
;********************************************************************	

EE_WR	bank1
;	movlw	EE_ADR
	MOVWF	EEADR		; Address to write

	MOVF	EEDATA0,W	; Data byte #0
	CALL	EE_W

	MOVF	EE_DATA1,W	; Data byte #1
	CALL	EE_Winc

	bank0
	RETURN

	errorlevel	-302	; In Bank 2

EE_Winc	INCF	EEADR,F		; bump address
	
EE_W	MOVWF	EEDATA
	BSF	EECON1,WREN	; Enable Write
	MOVLW	0x55		;
	MOVWF	EECON2		; Write 0x55
	MOVLW	0xAA		;
	MOVWF	EECON2		; Write 0xAA
	BSF	EECON1,WR	; Set WR bit (begin write)

EE_W2	BTFSC	EECON1,WR	; Wait for write to finish
	GOTO	EE_W2

	bank0
	BCF	PIR1,EEIF	; clear interrupts
	bank1
	RETURN	

	errorlevel	+302

;********************************************************************
 	END


