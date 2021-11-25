TITLE CS271 Project 6: Low level I/O and macros     (Proj6_BrogdonK.asm)

; Author: Kyle Brogdon
; Last Modified: 21NOV2021
; OSU email address: ONID_ID@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 06DEC2021
; Description: This file is provided as a template from which you may work
;              when developing assembly projects in CS271.

INCLUDE Irvine32.inc

mGetString MACRO prompt, stringStorage, bytesRead
	; Display a prompt (input parameter, by reference), then get the user’s keyboard input into a memory location (output parameter, by reference). 
	; You may also need to provide a count (input parameter, by value) for the length of input string you can accommodate and a provide a number 
	; of bytes read (output parameter, by reference) by the macro.

	PUSHAD

	MOV		EDX, prompt
	CALL	writeString
	MOV		EDX, stringStorage
	MOV		ECX, MAXSIZE
	CALL	readString
	MOV		EDI, bytesRead
	MOV		[EDI], EAX

	POPAD
ENDM

mDisplayString MACRO
	; Print the string which is stored in a specified memory location (input parameter, by reference).
ENDM

NUMINTS = 10										; number of ints we must collect from the user
MAXSIZE = 100										; max number of characters that can fit in a 32 bit register, including a leading sign
registerUpperLimit = 2147483647						; max value that can be accepted into a signed 32 bit register
registerLowerLimit = 2147483648						; (negated) minimum value that can be accepted into a signed 32 bit register

.data
	signedArray			SDWORD		NUMINTS DUP (?)
	userInputString		SBYTE		MAXSIZE DUP (?)
	introProgram1		BYTE		"Computer Architecture and Assembly Project 6: Low level input/output procedures and macros", 0
	introProgram2		BYTE		"Written by: Kyle Brogdon", 13, 10, 0
    programRules1		BYTE		"Please enter ", 0
	programRules2		BYTE		" signed decimal integers. Each number must fit inside a 32 bit register.", 0
	programRules3		BYTE		"After you have finished entering the signed numbers, I will display a list of the integers, their sum, and average.", 13, 10, 0
	userInputPrompt		BYTE		"Please enter a signed number: ", 0
	userInputNumeric	SDWORD		?
	errorPrompt			BYTE		"ERROR: You did not enter a signed number or the value was too large. Please try again.", 13, 10, 0
	farewell			BYTE		"Goodbye, and thanks for using this program!", 13, 10, 0
	stringLen			DWORD		?
	isNegative			DWORD		0
	integerCount		DWORD		0											; keeps track of number of integers in the signedArray in increments of 4 for DWORD

.code

;-----------------------------------------------------------------------------------------------
; Name: main
;
; does things
;
; Preconditions: Irvine32 must be included, NUMINTS must be declared, and .data must contain all arrays/strings
;										used by subprocedures. Array must be type BYTE.
;
;-----------------------------------------------------------------------------------------------

main PROC
	; introduces the user to the program, the programmer, and the rules
	PUSH	OFFSET programRules3
	PUSH	OFFSET programRules2
	PUSH	OFFSET programRules1
	PUSH	OFFSET introprogram2
	PUSH	OFFSET introProgram1
	CALL	introduction

	MOV		ECX, NUMINTS
_mainLoop:
	PUSH	OFFSET integerCount
	PUSH	OFFSET signedArray
	PUSH	OFFSET errorPrompt
	PUSH	OFFSET userInputNumeric
	PUSH	OFFSET isNegative
	PUSH	OFFSET stringLen
	PUSH	OFFSET userInputString
	PUSH	OFFSET userInputPrompt
	CALL	readVal

	CALL	writeVal

	LOOP	_mainLoop
	JMP		_sayGoodbye

_sayGoodbye:
	; says farewell to the user
	PUSH	OFFSET farewell						
	CALL	goodbye								

	Invoke ExitProcess,0	; exit to operating system
main ENDP

;-----------------------------------------------------------------------------------------------
; Name: introduction
;
; Takes multiple string inputs, then prints those strings to introduce the program and the programmer 
;	to the user and displays the rules of the program to the user.
;
; Preconditions: NUMINTS must be declared. Irvine32 lib must be included.
;
; Receives:
;				
;				[EBP + 24]			= programRules3 passed by reference
;				[EBP + 20]			= programRules2 passed by reference
;				[EBP + 16]			= programRules1 passed by reference
;				[EBP + 12]			= introProgram2 passed by reference
;				[EBP + 8]			= introProgram1 passed by reference
;-----------------------------------------------------------------------------------------------

introduction PROC
	PUSH	EBP
	MOV		EBP, ESP
	PUSH	EDX
	PUSH	EAX

	;print introProgram1 and introProgram2
	MOV		EDX, [EBP + 8]
	CALL	writeString
	CALL	CrLf
	MOV		EDX, [EBP + 12]
	CALL	writeString
	CALL	crLf
	
	;print programRules1 and number of desired ints
	MOV		EDX, [EBP + 16]						
	CALL	writeString
	MOV		EAX, NUMINTS
	CALL	writeDec


	;print programRules2
	MOV		EDX, [EBP + 20]
	CALL	writeString
	CALL	CrLF

	;print programRules3
	MOV		EDX, [EBP + 24]
	CALL	writeString
	CALL	CrLF

	; restore stack
	POP		EDX
	POP		EAX
	POP		EBP
	RET		20
introduction ENDP

;-----------------------------------------------------------------------------------------------
; Name: readVal
;
;
; Preconditions: 
;
; Postconditions: 
;
; Receives:
;				[EBP + 36]			= integerCount
;				[EBP + 32]			= signedArray
;				[EBP + 28]			= errorPrompt
;				[EBP + 24]			= userInputNumeric
;				[EBP + 20]			= isNegative
;				[EBP + 16]			= stringLen
;				[EBP + 12]			= userInputString
;				[EBP + 8]			= userInputPrompt
;
; Returns: None	
;-----------------------------------------------------------------------------------------------

readVal PROC
	PUSH	EBP
	MOV		EBP, ESP
	PUSHAD

_getString:
	mGetString [EBP + 8], [EBP + 12], [EBP + 16]

	; check first digit for a sign and set up loop
	CLD	
	MOV		ECX, [EBP + 16]
	MOV		EBX, 0
	CMP		[ECX], EBX
	JE		_error							; means the user provided no input
	MOV		ESI, [EBP + 12]					; move string of digits into ESI
	LODSB
	CMP		AL, 43
	JE		_loopSetup
	CMP		AL, 45
	JE		_setNegative
	MOV		ESI, [EBP + 12]					; reset ESI because there is no sign in front of the string
	JMP		_loopSetup
	

_setNegative:
	; set isNegative flag
	MOV		EDX, [EBP + 20]
	MOV		EBX, 1
	MOV		[EDX], EBX
	MOV		EDI, [ECX]
	DEC		EDI
	MOV		ECX, EDI
	MOV		EBX, [EBP + 24]
	JMP		_convertToNumLoop

_loopSetup:
	MOV		EBX, [EBP + 24]
	MOV		EDI, [ECX]
	MOV		ECX, EDI

_convertToNumLoop:
	PUSH	ECX
	LODSB									; puts first digit in AL
	CMP		AL, 48
	JB		_error
	CMP		AL, 57
	JA		_error
	CLC
	MOVZX	EDI, AL							; copy value of AL
	MOV		EAX, [EBX]
	MOV		ECX, 10
	MOV		EDX, 0
	MUL		ECX
	POP		ECX								; restore loop counter
	CMP		EDX, 0
	JNZ		_error							; if not 0, it is too big for 32 bit register
	SUB		EDI, 48
	ADD		EAX, EDI
	JC		_error							; if carry is set, then too large
	MOV		[EBX], EAX
	CMP		EAX, registerUpperLimit
	JA		_edgeCase
	LOOP	_convertToNumLoop
	JMP		_checkNegative

_edgeCase:
	; handle case where it is positive and > 2147483647	
	MOV		ECX, [EBP + 20]
	MOV		EDX, [ECX]
	CMP		EDX, 0
	JE		_error				; if positive and > upperLimit, then error
	CMP		EAX, registerLowerLimit
	JLE		_isNegative
	JMP		_error
	

_checkNegative:
	MOV		EAX, [EBP + 20]
	MOV		EDX, [EAX]
	CMP		EDX, 1
	JNE		_addToArray

_isNegative:
	MOV		EAX, [EBX]
	NEG		EAX
	CMP		EAX, registerLowerLimit
	JL		_error
	MOV		[EBX], EAX
	JMP		_addToArray

_error:
	MOV		EDX, [EBP + 28]					; write error message
	CALL	writeString
	MOV		EDX, [EBP + 20]
	MOV		EBX, 0
	MOV		[EDX], EBX						; reset value of isNegative
	MOV		EDX, [EBP + 24]
	MOV		[EDX], EBX						; reset value of userInputNumeric
	JMP		_getString						; reprompt user
	

_addToArray:
	MOV		EAX, [EBP + 36]
	MOV		ECX, [EAX]
	MOV		ESI, [EBP + 32]
	MOV		EDX, [EBX]
	MOV		[ESI + ECX], EDX
	ADD		ECX, 4
	MOV		[EAX], ECX



_restoreStack:							
	MOV		EDX, [EBP + 20]
	MOV		EBX, 0
	MOV		[EDX], EBX						; reset value of isNegative
	MOV		EDX, [EBP + 24]
	MOV		[EDX], EBX						; reset value of userInputNumeric
	POPAD
	POP		EBP
	RET		32
readVal ENDP

;-----------------------------------------------------------------------------------------------
; Name: writeVal
;
;
; Preconditions: 
;
; Postconditions: 
;
; Receives:
;				[EBP + 20]			=
;				[EBP + 16]			= stringLen
;				[EBP + 12]			= userInputString
;				[EBP + 8]			= userInputPrompt
;
; Returns: None	
;-----------------------------------------------------------------------------------------------

writeVal PROC

RET
writeVal ENDP

;-----------------------------------------------------------------------------------------------
; Name: goodbye
;
; Takes a string, then says goodbye and thank you to the user.
;
; Preconditions: Irvine32 must be included.
;
; Receives:
;
;				[EBP + 8]			= goodbye string passed by reference
;
;-----------------------------------------------------------------------------------------------
goodbye PROC
	PUSH	EBP
	MOV		EBP, ESP

	; print goodbye
	CALL	crLF
	PUSH	EDX
	MOV		EDX, [EBP + 8]
	CALL	writeString
	
	POP		EDX
	POP		EBP	
	RET		4
goodbye ENDP

END main
