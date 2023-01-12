;******************************************************************************
;******************************************************************************
;	PROGRAMA DO PROTOTIPO DA PLACA REPETIDORA COM ISOLOOP ( PIC16F627 )
;	Data: 05 de maio de 2008.
;	Vers�o: 2.0
; Nesta revis�o foi adicionada a palavra de configura��o dos FUSES no firmware,
; para que o programador carregue automaticamente as configura��es de grava��o.

INCLUDE "P16F628.INC"

DETECT_B	EQU	0h	;Pino RA0 (pino 17)
TRANSM_ENB	EQU	1h	;Pino RA1 (pino 18)
TRANSM_ENA	EQU	3h	;Pino RB3 (pino 9)
DETECT_A	EQU	5h	;Pino RB5 (pino 11)

;***************************************
; A SRAM comeca a partir do endereco 20H

FlagSys		EQU	20H	;Registrador de flags do sistema
DadoTransmitido	EQU	0h	;Flag=1: trasmitido recentemente.
				;Flag=0: linha em sil�ncio por mais de 2ms.

;-------------------------------------------------------------------------------------
;Definic�es para o compilador gerar o arquivo .HEX j� com as configura��es de grava��o
;deste programa.
        ORG     0x2007
        dw      b'00001000000010'

;********************************
	ORG	0000h		;Vetor de reset
	goto	INICIO		;

	ORG	0004h				;Vetor comum `as interrupcoes.
	btfsc	PIR1,RCIF		;Testa se ocorreu Int. Serial de recep��o.
	goto	DADO_RECEBIDO	;Trata Recep��o Serial.
	btfsc	INTCON,INTF		;Testa se ocorreu Int. Externa.
	goto	RESP_ATUADOR	;Trata Int. Externa.
	btfsc	INTCON,T0IF		;Testa se ocorreu Int. de overflow do Timer0.
	goto	FimTransmissao	;Trata Int. de Timer0
	retfie					;Retorna caso ocorra uma interrup��o n�o definida.

;********************************
;*** INICIALIZACAO DO SISTEMA ***
;********************************
INICIO
	clrwdt				;Zera WDT e prescaler
	clrf	PORTA			;Zera latchs da PORTA
	clrf	PORTB			;Zera latchs da PORTB
	movlw	b'00000111'		;|Desliga comparadores
	movwf	CMCON			;|
	bsf		STATUS,RP0		;Seleciona Banco 1
	movlw	b'00111101'		;|Configura os pinos das portas
	movwf	TRISA			;|
    movlw   b'01100111'     ;|Configura Pinos INT, Rx e Tx como entradas
							;|RA1 e RB3 s�o saidas de controle de fluxo
							;|RA0 e RB5 s�o entradas de detec��o de sinal
    movwf   TRISB			;|ASYNC, RBIF
    movlw   b'01100110'		;|Habilita Tx, BRGH high, dado de 9 bits, assincrono
	movwf	TXSTA			;|
	movlw	d'95'			;|Taxa de 9600, para um cristal de 14,7456MHz
	movwf	SPBRG			;| BR=Fosc/[16*(SPBRG+1)] , quando BRGH=1(high)
	movlw	b'00000110'		;|Seleciona pull-up porta RB, prescaler=128, INT
	movwf	OPTION_REG		;|externa por descida e clock interno para timer.
    movlw   b'00100000'		;|Habilita int Recep��o
	movwf	PIE1			;|
	bcf	STATUS,RP0		;Seleciona Banco 0
	movlw	b'01010000'		;|Desmascara int�s dos dispositivos perif�ricos
	movwf	INTCON			;|INT Externa e Serial(Rx)
	bcf	PORTB,TRANSM_ENA	;|Configura ambos canais para recep��o.
	bcf	PORTA,TRANSM_ENB	;|
	bcf	FlagSys,DadoTransmitido	;Indica sil�ncio na linha.
    movlw   b'11010000'		;|Ativa porta serial, dado de 9 bits, recepcao continua,
	movwf	RCSTA			;|sem detec��o de endere�o, sem detec��o de erros.

	bsf	INTCON,GIE		;Desmascara todas as int�s habilitadas.

;////////////////////////////////////////////////////////////////////////////
;*******************************
; PROGRAMA PRINCIPAL
Loop_deteccao
;Testa_CANALA
	btfss	PORTB,DETECT_A
	goto	CanalA_Ativo
;Testa_CANALB
	btfss	PORTA,DETECT_B
	goto	CanalB_Ativo
	goto	Loop_deteccao	;Mantem-se em loop enquanto linha em sil�ncio.
;-----------
CanalA_Ativo
	bsf	PORTA,TRANSM_ENB
	goto	DisparaTimer
CanalB_Ativo
	bsf	PORTB,TRANSM_ENA
DisparaTimer
	bcf		INTCON,T0IF	;|Zera Flag de Int do timer0, carrega e habilita
	movlw	d'220'		;|timer para 1ms.
	movwf	TMR0		;|
	bsf		INTCON,T0IE	;|
	goto	Loop_deteccao	;Entra em loop at� concluir a recep��o.


;**************************************************
; Rotina de servi�o de interrup��o externa
; A resposta do atuador gera um interrup��o externa
RESP_ATUADOR
	bsf	PORTB,TRANSM_ENA	;|Configura todos os canais para
	bsf	PORTA,TRANSM_ENB	;|transmiss�o.
	bcf	INTCON,INTF		;Limpa obrigatoriamente a flag de Int. Externa
	goto	LiberaTempo

;********************************************************
; Rotina de servi�o de interrup��o serial (dado recebido)
; O recebimento de um dado gera uma interrup��o serial
DADO_RECEBIDO
	btfss	FlagSys,DadoTransmitido
	goto	EstavaEmSilencio
	btfss	PIR1,TXIF
	goto	DADO_RECEBIDO	;Espera esvaziar buffer de transmiss�o 
EstavaEmSilencio

	bsf		TXSTA,TX9D	;|Seta o bit de paridade de transmiss�o, mas...
	btfss	RCSTA,RX9D	;|...se o bit de paridade recebido for 0...
	bcf		TXSTA,TX9D	;|...ent�o zera tambem o bit de paridade a ser transmitido.
						;|...sen�o, deixa o bit de paridade em 1.
	movfw	RCREG		;|Transfere o byte recebido para o buffer de transmsiss�o.
	movwf	TXREG		;|

	bsf	FlagSys,DadoTransmitido
LiberaTempo
	movlw	d'198'		;|Zera Flag de Int do timer0, carrega e habilita
	movwf	TMR0		;|timer para 2ms.
	bcf		INTCON,T0IF	;|
	bsf		INTCON,T0IE	;|
	retfie

;******************************
FimTransmissao
	bcf	INTCON,T0IE		;Desabilita Int de Timer0
	bcf	INTCON,T0IF		;Zera flag de Timer0
	bcf	PORTB,TRANSM_ENA	;|Configura todos os canais para
	bcf	PORTA,TRANSM_ENB	;|recep��o.
	bcf	FlagSys,DadoTransmitido	;Indica sil�ncio na linha.
	retfie

	END
