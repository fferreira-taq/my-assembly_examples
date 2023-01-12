;*******************************************************************
;
;	Inductance & Capacitance Meter with Software calibration
;
;*******************************************************************
;
;	First, let us choose our weapon - a 16F628
;
;*******************************************************************
;
; LC002	- THIS ONE WORKS FINE WITH A WELL BEHAVED DISPLAY
;
;	Deleted  CpyBin subroutine and one call to it
;
;	Modified B2_BCD to take its input directly from <AARGB0,1,2>
;
;	Modified "oscillator test" so it copies F3   to <AARGB0,1,2>
;
;	Fixed Get_Lcal so it gets the correct number
;
;	Minor adjustment to MS100 timing to correct frequency display
;
;	Check for oscillator too slow when measuring L or C.
;
;
;*******************************************************************
;
; LC003	- Optimised / Modified to handle "bad" displays
;
;	Removed duplicated code in DATS subroutine
;
;	Added code to fix crook display (select by jumper on B4 - 10)
;
;	Optimised L & C formatting code
;
;	Optimised "Display" subroutine
;
;	Cleaned up LCDINIT
;
;
;*******************************************************************
;
; LC004 - Deleted timer Interrupt Service Routine
;
;	Modified way oscillator "out of range" condition is detected
;
;
;*******************************************************************
;
; LC628 - LC004 code ported to 16F628 by Egbert Jarings PA0EJH.
;	Mem starts now at 0x20
;	InitIO modified , 628 PortA start's up in Analog Mode 
;	So changed to Digital Mode (CMCON)
; 
;	Display's "Calibrating" to fill up dead Display time
;	when first Powerd Up.
;
;	Changed pmsg Routine, EEADR  trick  wont work with 628,
;	PCL was always 0x00 so restart occurs. EEADR is now Etemp.
;
;	Also changed EEADR in FP routine to Etemp 
;
;	Bad Display isn't bad at all, its a Hitachi HD44780, as
;	80% of all Display's are. Adress as 2 Lines x 8 Char.
;	So LCDINIT modified for 2 x 8 Display's. (0x28 added)
;
;*******************************************************************
;
; LC005 - Cosmetic rewrite of RAM allocation from LC004
;
;	No change to address of anything - I hope
;	Identified unused RAM & marked for later removal.
;
;
;*******************************************************************
;
; LC006 - Merge LC005 and LC628
;
;	All "#ifdef" F628 parts by Egbert Jarings PA0EJH.
;	(or derived from his good work)
;
;	Cleaned up RAM allocation.
;
;	Added message re: processor type, just to verify selection
;
;	Included extra initialisation (2 line) command by PA0EJH
;
;*******************************************************************
;
; lc007	Changed strings to EEPROM (it's not used for anything else)
;
;	Added "error collector" code to catch "all" FP errors
;
;       Addded macros
;
;
;*******************************************************************
;
; LC_swcal.000
;	Changed to use only F628 processor
;	Used internal comparator of F628 in place of LM311
;	Switched relay directly by digital I/O
;	Implemented software calibration via constant in EEPROM
;	Re-allocated most I/O pins
;	Added output munger for LCD connections (easy to re-allocate)
;
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

#define	LCD0	PORTB,3
#define	LCD1	PORTB,2
#define	LCD2	PORTB,1
#define	LCD3	PORTB,0
#define	ENA	PORTB,4		; Display "E"
#define	RS	PORTB,5		; Display "RS"
#define	functn	PORTB,6		; 0 = "Inductor"
#define relay	PORTB,7		; Switches Ccal

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

	cal_t:2			; Ccal temporary value

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

	org	0

GO	clrwdt			; 0 << Reset
	call	InitIO
	bcf	relay		; Remove Ccal

;**********************************************************
;
;	Main Program
;

START	CALL	LCDINIT	 	; INITIALIZE LCD MODULE	
	call	EE_RD		; Retrieve CCal integer value

cmdloop	call	HOME

;
;	"Zero" the meter.
;

Chk4Z	MOVLW   Calibr-0x2100	; Display's " Calibrating "
	call	pmsg		; to entertain the punters

	call	Measure		; Dummy Run to stabilise oscillator.
	call	MS200

	call	Measure		; Get freq in F3

	copy	F3+0,F1+0	; Copy F3 to F1
	copy	F3+1,F1+1

	bsf	relay		; Add standard capacitor
	call	MS200

	call	Measure		; Get freq in F3

	copy	F3+0,F2+0	; Copy F3 to F2
	copy	F3+1,F2+1
	
	bcf	relay		; Remove standard capacitor
	call	MS200

	call	Measure		; Dummy Run to stabilise oscillator.

;
;	Now we resume our regular pogrom
;	Read state of user test links on LCD bus
;

M_F3	bank1			; PORTB:-
	movlw	b'01001111'	; LCD data bits to read
	movwf	TRISB		; 1 = input
	bank0			; 0 = output

	call	MS2		; Settling time
	copy	PORTB,links

	bank1
	movlw	b'01000000'	; restore data direction
	movwf	TRISB		; 1 = input
	bank0			; 0 = output

;---------------------------------------------------------------
;
;	Take a break from regular duties to do something interesting
;
	btfss	links,0		; Raise Ccal value
	goto	cal_up

	btfss	links,1		; Lower Ccal value
	goto	cal_dn

	btfss	links,2		; Test osc without Ccal
	goto	osc1

	btfss	links,3		; Test osc with Ccal
	goto	osc2

;
;	None of the above
;

	bcf	relay		; In case of osc test
	btfss	EEflag,0	; Time to save Ccal value?
	goto	cont		; No. Back to work
	
	bcf	EEflag,0	; To say we have done it
	call	EE_WR		; So, we better save it
	goto	cont		; Hi Ho, its off to work I go

;
;	Add +10 to cal_t:2
;

cal_up	bsf	EEflag,0	; Say "we're adjusting"
	movlw	0x0a		; +10
	addwf	cal_t+1,f
	bcc	cont
	
	incf	cal_t+0,f
	goto cont

;
;	Add -10 to cal_t:2
;

cal_dn	bsf	EEflag,0	; Say "we're adjusting"
	movlw	0xf6		; -10
	addwf	cal_t+1,f
	bcc	hi_byte
	
	incf	cal_t+0,f

hi_byte	movlw	0xff
	addwf	cal_t+0,f
	goto	cont		

;
;	Measure & display osc freq for initial setup
;

osc2	bsf	relay		; Add Ccal
	
osc1	call	HOME
	call	Measure		; Measure Local Osc Freq.
	call	CLEAR

	btfss	INTCON,T0IF	; Set = Counter overflow?
	goto	Do_Disp

	MOVLW	ovr-0x2100	; Over-range message
	call	pmsg
	goto	M_F3
	
Do_Disp	clrf	AARGB0		; Copy to 24 bit number
	movf	F3,W		; in AARGB0, 1, 2
	movwf	AARGB1		; for display
	movf	F3+1,W
	movwf	AARGB2

	call	Display
	goto	M_F3

;---------------------------------------------------------------

cont	call	HOME
	call	MS200
	call	Measure		; Measure F3 & leave it there

	movf	F3,w		; test for "too low" frequency
	beq	OORange		; F < 2560Hz ?
	
	btfss	INTCON,T0IF	; test for "too high" frequency
	goto	OK2GO		; F > 655359Hz ?

OORange	MOVLW	ovr-0x2100	; Over/Under range message
	call	pmsg
	
	goto	M_F3

;
;	Precompute major bracketed terms cos
;	we need 'em both for all calculations
;

OK2GO	clrf	FPE		; Declare "error free"
	call	F1_F2
	call	F1_F3

;
;	See what mode we are in
;

	btfss	functn		; 0=Inductor
	goto	Do_Ind

;
;	OK, we've been told it's a capacitor
;

Do_Cap	call	C_calc
	movf	FPE,f		; Any FP errors?
	bne	complain
	
	movlw	Cintro-0x2100	; C =
	call	pmsg

	call	C_disp
	goto	M_F3

;
;	Now, they reckon it's a @#$*! inductor
;

Do_Ind	call	L_calc
	movf	FPE,f		; Any FP errors?
	bne	complain
	
	movlw	Lintro-0x2100	; L =
	call	pmsg

	call	L_disp
	goto	M_F3

;
;	Got a Floating Point Error of some sort
;

complain	movlw	ovr-0x2100	; Over Range
		call	pmsg
	
		goto	M_F3

;**********************************************************
;
;	Print String addressed by W
;	Note: Strings are in EEPROM
;	We do a lotta bank switching here.

pmsg	bank1
	movwf	EEADR		; pointer

pm1	BSF     EECON1,RD       ; EE Read
        MOVF    EEDATA,W        ; W = EEDATA, affects Z bit
        bank0			; Does not change Z bit
 
	btfsc	STATUS,Z	; ZERO = All done
	return			; so quit

	call	DATS		; Byte -> display

	bank1
	INCF    EEADR,F         ; bump address
	goto	pm1

;**********************************************************
;
;	Delay for 2ms (untrimmed)
;

MS2	MOVLW	0xFD		; DELAY 2ms
	MOVWF	COUNT1

	MOVLW	0x66
	MOVWF	COUNT2

	goto	L3		

;**********************************************************
;
;	Delay for about 200ms or 300ms (untrimmed)
;

MS300	call	MS100

MS200	call	MS100

;**********************************************************
;
;	Delay for about 100ms
;

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
;	Put a BCD nybble to display
;

PutNyb	ANDLW	0x0F		; MASK OFF OTHER PACKED BCD DIGIT
	ADDLW	0x30		; Convert BIN to ASCII

;**********************************************************
;
;	Put a byte to display
;

DATS	decf	TabStop,F	; Time to tickle bad display?
	bne	DAT1		; Not yet
	
	movwf	TabTemp		; Save character
	
;	btfss	FIXIT		; Check if we got a crook one.
;	CALL	LINE2		; Skip this if good

	movf	TabTemp,W	; Restore character

DAT1	BSF	RS		; SELECT DATA REGISTER
CM	MOVWF	CHR		; STORE CHAR TO DISPLAY
	SWAPF	CHR,W		; SWAP UPPER AND LOWER NIBBLES (4 BIT MODE)

	call	PB_dly

	MOVF	CHR,W		; GET CHAR AGAIN 

;**********************************************************
;
;	Put 4 bits to LCD & wait (untrimmed)
;

PB_dly	movwf	PB_data		; Save nybble

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

;	goto	D200us		; Fall into DELAY subroutine

;**********************************************************
;
;	Delay for 200us (untrimmed)
;

D200us	MOVLW	0x42		; DELAY  200us
	MOVWF	COUNT1	
NXT5	DECFSZ	COUNT1,F
	GOTO	NXT5	
	RETLW	0

;******************************************************************
;
;	Convert 24-bit binary number at <AARGB0,1,2> into a bcd number
;	at <bcd>. Uses Mike Keitz's procedure for handling bcd 
;	adjust; Modified Microchip AN526 for 24-bits.
;

B2_BCD

b2bcd   movlw   .24		; 24-bits
        movwf   COUNT		; make cycle counter

        clrf    bcd+0		; clear result area
        clrf    bcd+1
        clrf    bcd+2
        clrf    bcd+3
        
b2bcd2  movlw   bcd 		; make pointer
        movwf   FSR
        movlw   .4
        movwf   cnt

; Mike's routine:

b2bcd3  movlw   0x33            
        addwf   INDF,f          ; add to both nybbles
        btfsc   INDF,3          ; test if low result > 7
        andlw   0xf0            ; low result >7 so take the 3 out
        btfsc   INDF,7          ; test if high result > 7
        andlw   0x0f            ; high result > 7 so ok
        subwf   INDF,f          ; any results <= 7, subtract back
        incf    FSR,f           ; point to next
        decfsz  cnt,f
        goto    b2bcd3
        
        rlf     AARGB2,f	; get another bit
        rlf     AARGB1,f
        rlf     AARGB0,f

        rlf     bcd+3,f         ; put it into bcd
        rlf     bcd+2,f
        rlf     bcd+1,f
        rlf     bcd+0,f

        decfsz  COUNT,f         ; all done?
        goto    b2bcd2          ; no, loop
        return                  ; yes


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
Home2	CALL	STROBE
	goto	MS2


;**********************************************************
;
;	SENDS DATA TO LCD DISPLAY MODULE (4 BIT MODE)	
;

STROBE	BCF	RS		; SELECT COMMAND REGISTER
	GOTO	CM


;************ MOVE TO START OF LINE 2 *****************

LINE2	MOVLW	0xC0		; ADDRESS FOR SECOND LINE OF DISPLAY

ST200us	CALL	STROBE
	goto	D200us


;********************************************************************
;       Initialise Input & Output devices
;********************************************************************

InitIO	movlw	b'00000110'
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

				; PORTA:-
	movlw	b'11110111'	; initialise data direction
				; 1 = input
				; 0 = output
								;
	movwf	TRISA		; PORTA<0>   = comp1 "-" in
				; PORTA<1>   = comp2 "-" in
				; PORTA<2>   = comp1&2 "+" in
				; PORTA<3>   = comp1 out
				; PORTA<4>   = comp2 out, T0CKI in
				; PORTA<5:7> = unused
				;
				;
				; PORTB:-
	movlw	b'01000000'	; initialise data direction
				; 1 = input
				; 0 = output
				;
	movwf	TRISB		; PORTB<0>   = LCD out "DB4"
				; PORTB<1>   =         "DB5"
				; PORTB<2>   =         "DB6"
				; PORTB<3>   =         "DB7"
				; PORTB<4>   = E  out to LCD
				; PORTB<5>   = RS out to LCD
				; PORTB<6>   = function in
				; PORTB<7>   = Ccal switch out

	bank0

	return	

;**********************************************************
;
;	Measure Frequency. Stash in "F3 and F3+1"
;

Measure	bcf	INTCON,T0IF	; Declare "Not yet Over-range"
	CLRF	TMR0		; RESET INTERNAL COUNT (INCLUDING PRESCALER)
				; See page 27 Section 6.0

	CLRF	F3		; Ready to receive 16 bit number
	CLRF	F3+1

	bank1
				; OPEN GATE
	
	movlw	b'11100111'	; Enable RA4 output to T0CKI
	movwf	TRISA		; 1 = input
				; 0 = output
	
	CALL	MS100		; 100MS DELAY

				; CLOSE GATE (COUNT COMPLETE)

	movlw	b'11110111'	; Disable RA4 output to T0CKI
	movwf	TRISA		; 1 = input
				; 0 = output
	bank0

	MOVF	TMR0,W		; GET HIGH BYTE		
	MOVWF	F3		; Copy to Big end of 16 bit result

; The comparator is "outputting" a 1 'cos we've forced it high
; so T0CKI=1.

PSC1	bank1
	bsf	OPTION_REG,T0SE	; Clock the prescaler
	nop
	bcf	OPTION_REG,T0SE
	bank0
	
	DECF	F3+1,F		; Decrement the counter
	
	movf	TMR0,W		; Has TMR0 changed?
	xorwf	F3,W		; if unchanged, XOR -> 0

	beq	PSC1

	return			; F3 : F3+1 now holds 16 bit result

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

;**********************************************************
;
;	Formatted display of BCD work area for Capacitor
;

C_disp	movf	R_sign,w	; Sign
	call	DATS

F_C1	MOVF	bcd+0,W
	ANDLW	0x0F
	beq	F_C2

	CALL	PutNyb
	call	Swap1
	call	Move1
	CALL	DoDP		; Print DP
	call	Swap2
	goto	F_C3U

;--------------------------------------------------

F_C2	swapf	bcd+1,W
	ANDLW	0x0F
	beq	F_C3

	CALL	PutNyb
	call	Move1
	CALL	DoDP		; Print DP
	call	Swap2
	call	Move2
	goto	F_C3U		; print nF. includes RETURN

;--------------------------------------------------

F_C3	MOVF	bcd+1,W
	ANDLW	0x0F
	beq	F_C4

	CALL	PutNyb
	CALL	DoDP		; Print DP
	call	Swap2
	call	Move2
	call	Swap3

F_C3U	movlw	Unit1-0x2100	; nF
	goto	pmsg		; includes RETURN

;--------------------------------------------------

F_C4	SWAPF	bcd+2,W		; Digit1 == 0 ?
	ANDLW	0x0F
	bne	NoB1_C

	MOVLW	0x20		; YES PRINT A SPACE
	call	DATS

	MOVF	bcd+2,W		; Digit2 == 0 ?
	ANDLW	0x0F
	bne	NoB2_C

	MOVLW	0x20		; YES PRINT A SPACE
	call	DATS
	bra	NoB3_C

NoB1_C	call	Swap2		; 1
NoB2_C	call	Move2		; 2
NoB3_C	call	Swap3		; 3
	CALL	DoDP		; Print DP
	call	Move3		; 4

	movlw	Unit2-0x2100	; pF
	goto	pmsg		; includes RETURN

;**********************************************************
;
;	Formatted display of BCD work area for Inductor
;

L_disp	movf	R_sign,w	; Sign
	call	DATS

F_L1	MOVF	bcd+0,W
	ANDLW	0x0F
	beq	F_L2

	CALL	PutNyb
	call	Swap1
	CALL	DoDP		; Print DP
	call	Move1
	call	Swap2
	goto	F_L2U		; Print mH. includes RETURN

;--------------------------------------------------

F_L2	swapf	bcd+1,W
	ANDLW	0x0F
	beq	F_L3

	CALL	PutNyb
	CALL	DoDP		; Print DP
	call	Move1
	call	Swap2
	call	Move2
	
F_L2U	movlw	Unit3-0x2100	; mH
	goto	pmsg		; includes RETURN

;--------------------------------------------------

F_L3	MOVF	bcd+1,W
	ANDLW	0x0F
	beq	F_L4

	CALL	PutNyb
	call	Swap2
	call	Move2
	CALL	DoDP		; Print DP
	call	Swap3
	goto	F_L4U		; Print uH. includes RETURN

;--------------------------------------------------

F_L4	SWAPF	bcd+2,W		; Digit1 == 0 ?
	ANDLW	0x0F
	bne	NoB1_L

	MOVLW	0x20		; YES PRINT A SPACE
	call	DATS

	goto	NoB2_L

NoB1_L	call	Swap2		; 1
NoB2_L	call	Move2		; 2
	CALL	DoDP		; Print DP
	call	Swap3		; 3
	call	Move3		; 4

F_L4U	movlw	Unit4-0x2100	; uH
	goto	pmsg		; includes RETURN

;--------------------------------------------------
;
;	Common subroutine for formatted output
;

DoDP	MOVLW	'.'		; Print DP
	goto	DATS		; Return from DATS

Swap0	SWAPF	bcd+0,W		; GET NEXT DIGIT
	goto	PutNyb		; DISPLAY IT

Move0	MOVF	bcd+0,W		; GET OTHER BCD DIGIT
	goto	PutNyb

Swap1	SWAPF	bcd+1,W
	goto	PutNyb

Move1	MOVF	bcd+1,W
	goto	PutNyb

Swap2	SWAPF	bcd+2,W
	goto	PutNyb

Move2	MOVF	bcd+2,W
	goto	PutNyb

Swap3	SWAPF	bcd+3,W
	goto	PutNyb

Move3	MOVF	bcd+3,W
	goto	PutNyb

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
;	Calculate Unknown Capacitance OR inductance
;	
;       Output: 24 bit positive integer (scaled)
;	right justified in AARGB0, AARGB1, AARGB2
;	also as BCD in bcd:bcd+1:bcd+2:bcd+3
;
;********************************************************************	

C_calc	call	divide
	call	Get_Ccal	; Times 10,000 ( = 1000.0pF)
	call	multiply
	goto	PorM		; includes return

;--------------------------------------------------------------------

L_calc	call	multiply
	call	Get_Lscale	; Precomputed Scale_factor/(4*PI*PI)
	call	multiply
	call	Get_Ccal
	call	S_swap
	call	divide

L_divF1	call	Get_F1		; Divide by F1^2
	call	S_push
	call	multiply
	call	S_swap
	call	divide

;
;	Handle Space or Minus in front of FP number
;	

PorM	btfss	AARGB0,7	; test sign
	goto	Pplus
	
Pminus	movlw	0x2d		; minus
	goto	PMdisp

Pplus	movlw	0x20		; plus

PMdisp	movwf	R_sign		; save for later display
	bcf	AARGB0,7	; make plus anyway

;
;	Format as raw BCD string in bcd:bcd+1:bcd+2:bcd+3
;

	call	INT2424		; To INT in AARGB0 etc.
	iorwf	FPE,f		; W may hold Error (0xff)
	goto	B2_BCD		; includes return

;********************************************************************
;
;	Calculate (F1/F3)^2-1, leave result on stack
;
;********************************************************************	

F1_F3	call	Get_F3
	goto	F1_F1

;********************************************************************
;
;	Calculate (F1/F2)^2-1, leave result on stack
;
;********************************************************************	

F1_F2	call	Get_F2
F1_F1	call	Get_F1
	call	divide		; F1/Fx
	call	S_push
	call	multiply	; (F1/Fx)^2
	call	Get_One
	call	S_swap
	goto	subtract	; (F1/Fx)^2-1
				; includes return

;********************************************************************
;	Fetch assorted things used for the calculation
;	of Unknown L and C
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

Get_One		call	S_push		; make room first
		clrf	AEXP		; Create a binary 1
		clrf	AARGB0
		clrf	AARGB1
		movlw	0x01
		goto	LSB2stak
	

Get_Ccal	movlw	cal_t		; Get integer value
		goto	W2stak		; Includes stack push

Get_F1		movlw	F1		; Includes stack push
		goto	W2stak

Get_F2		movlw	F2		; Includes stack push
		goto	W2stak

Get_F3		movlw	F3		; Includes stack push
;		goto	W2stak

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
;	Read EEPROM into "cal_t"
;********************************************************************	

EE_RD	bank1
	movlw	cal_p-0x2100	; Address to read
	MOVWF	EEADR
	bank0

	CALL	EE_R
	MOVWF	cal_t+0

	CALL	EE_Rinc
	MOVWF	cal_t+1

	RETURN

EE_Rinc	bank1
	INCF	EEADR,F		; bump address

EE_R	bank1
	BSF	EECON1,RD	; EE Read
	MOVF	EEDATA,W	; W = EEDATA
	bank0

	RETURN


;********************************************************************
;	Write EEPROM from "cal_t"
;********************************************************************	

EE_WR	bank1
	movlw	cal_p-0x2100
	MOVWF	EEADR		; Address to write

	MOVF	cal_t+0,W	; Data byte #0
	CALL	EE_W

	MOVF	cal_t+1,W	; Data byte #1
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

	INCLUDE <FP.TXT>

;********************************************************************
;
;	Text Strings (stored in data EEPROM)
;

        ORG 0x2100

ovr	de	"   Over Range   ",0
Unit1	de	" nF",0
Unit2	de	" pF",0
Unit3	de	" mH",0
Unit4	de	" uH",0
Cintro	de	" C = ",0
Lintro	de	" L = ",0
Calibr  de	"   Calibrating  ",0

cal_p	de	0x27,0x10		; Initial value = 10000

 	END

;********************************************************************

