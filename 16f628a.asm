;******************************************************************************
;******************************************************************************
;	MODELO PADRÃO PARA INICIALIZAÇÃO DE MICROCONTROLADORS PIC16F628A PARA
;   PLATAFORMA DE DESENVOLVIMENTO MPLAB IDE V.8.36
;	DESENVOLVIDO POR FERNANDO FERREIRA
;	DATA: 03/10/2009



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

;********************************

	CBLOCK	0X20
	W_TEMP
	STATUS_TEMP
	ENDC

;*******************************************************
;	VETOR DE RESET DO SISTEMA                          | 
;*******************************************************
	ORG		0X00			;Vetor de reset
	goto	INICIO			;

;*******************************************************
;	INTERRUPÇÕES                                       |
;*******************************************************
	ORG		0X04			;Vetor comum `as interrupcoes.
	MOVWF	W_TEMP		; SALVA O REGISTRADOR W
	SWAPF	STATUS,W
	MOVWF	STATUS_TEMP	; SALVA O STATUS DO SISTEMA

INT_DEF:
	btfsc	PIR1,RCIF		;Testa se ocorreu Int. Serial de recepção.
	goto	INT_END			;Trata Recepção Serial.
	btfsc	INTCON,INTF		;Testa se ocorreu Int. Externa.
	goto	INT_END			;Trata Int. Externa.
	btfsc	INTCON,T0IF		;Testa se ocorreu Int. de overflow do Timer0.
	goto	INT_END			;Trata Int. de Timer0

INT_END:	
	SWAPF	STATUS_TEMP,W
	MOVWF	STATUS		; RETORNA O STATUS DO SISTEMA
	SWAPF	W_TEMP,F	; RETORNA O REGISTRADOR F
	SWAPF	W_TEMP,W	; RETORNA O REGISTRADOR W
	retfie					;Retorna caso ocorra uma interrupção não definida.
	
;********************************
;*** INICIALIZACAO DO SISTEMA ***
;********************************
INICIO
	clrwdt					;Zera WDT e prescaler
	clrf	PORTA			;Zera latchs da PORTA
	clrf	PORTB			;Zera latchs da PORTB
	movlw	b'00000111'		;|Desliga comparadores
	movwf	CMCON			;|
	BANK1					;|Seleciona banco 1	
	movlw	b'00111101'		;|Configura os pinos das portas
	movwf	TRISA			;|input=1
    movlw   b'01100111'     ;|output=0
    movwf   TRISB			;|
    
  	movlw	b'00000000'		;|
	movwf	OPTION_REG		;|
    BANK0					;Seleciona Banco 0
	movlw	b'00000000'		;|
	movwf	INTCON			;|
	bcf	INTCON,GIE			;|Mascara todas as int´s habilitadas.

;////////////////////////////////////////////////////////////////////////////
;*******************************
; PROGRAMA PRINCIPAL
end