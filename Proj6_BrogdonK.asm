TITLE CS271 Project 6: Low level I/O and macros     (Proj6_BrogdonK.asm)

; Author: Kyle Brogdon
; Last Modified: 21NOV2021
; OSU email address: ONID_ID@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 06DEC2021
; Description: This file is provided as a template from which you may work
;              when developing assembly projects in CS271.

INCLUDE Irvine32.inc

;-----------------------------------------------------------------------------------------------
; Name: mGetString
;
; Prompts user for a signed 32 bit integer
;
; Preconditions: Irvine32 must be included, three macro parameters are required, MAXSIZE must be declared.
;
; Postconditions: Values of prompt, stringStorage, bytesRead all changed.
;
; Receives:
;
;				prompt				= Address of a string to prompt user for input
;				stringStorage		= Address of a string array to store user input
;				bytesRead			= Address of a variable to store the number of bytes the user entered
;-----------------------------------------------------------------------------------------------

mGetString MACRO prompt:REQ, stringStorage:REQ, bytesRead:REQ
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
; Calls mGetString macro with three arguments to prompt user and get a signed 32 bit integer. Once 32bit int
;	is acquired from user, it is saved as a string of numerical digits, converted from ASCII to SDWORD, then
;	the input is validated as a number (no symbols or letters outside of + or -) that is correctly sized 
;	for signed 32bit. Once it is validated, it is stored in signedArray.
;
; Preconditions: Irvine32 must be included, registerUpperLimit and registerLowerLimit must be declared, 8 DWORD length
;					arguments must be passed via the stack.
;
; Postconditions: Values of userInputString, stringLen, signedArray, and integerCount are changed.
;
; Receives:
;				[EBP + 36]			= integerCount passed by reference
;				[EBP + 32]			= signedArray passed by reference
;				[EBP + 28]			= errorPrompt passed by reference
;				[EBP + 24]			= userInputNumeric passed by reference
;				[EBP + 20]			= isNegative passed by reference
;				[EBP + 16]			= stringLen passed by reference
;				[EBP + 12]			= userInputString passed by reference
;				[EBP + 8]			= userInputPrompt passed by reference
;
; Returns: None	
;-----------------------------------------------------------------------------------------------

readVal PROC
	PUSH	EBP
	MOV		EBP, ESP
	PUSHAD

_getString:
	; invoke get string and get user input
	mGetString [EBP + 8], [EBP + 12], [EBP + 16]

	; check if user entered anything
	CLD	
	MOV		ECX, [EBP + 16]					; stringLen into ECX
	MOV		EBX, 0
	CMP		[ECX], EBX
	JE		_error							; means the user provided no input

	; check if a sign was entered
	MOV		ESI, [EBP + 12]					; move string of numerical digits into ESI
	LODSB
	CMP		AL, 43							; check for plus sign
	JE		_leadingPlusSign
	CMP		AL, 45							; check for minus sign
	JE		_setNegative
	MOV		ESI, [EBP + 12]					; reset ESI because there is no sign in front of the string
	JMP		_loopSetup
	

_setNegative:
	; sets isNegative flag and sets up loop
	MOV		EDX, [EBP + 20]
	MOV		EBX, 1							
	MOV		[EDX], EBX						; set isNegative to 1
	MOV		EDI, [ECX]
	DEC		EDI								; decrease loop counter so stringLen does not count the sign
	MOV		ECX, EDI
	MOV		EBX, [EBP + 24]					; move userInputNumeric to EBX to store value once converted
	JMP		_convertToNumLoop

_leadingPlusSign:
	; handles where a + sign was put in front of the input
	MOV		EDI, [ECX]
	DEC		EDI
	MOV		ECX, EDI						; decrease loop counter so stringLen does not count the sign
	MOV		EBX, [EBP + 24]					; move userInputNumeric to EBX to store value once converted
	JMP		_convertToNumLoop

_loopSetup:
	; sets up userInputNumeric and loop counter
	MOV		EBX, [EBP + 24]
	MOV		EDI, [ECX]
	MOV		ECX, EDI

_convertToNumLoop:
	; converts string inputs to numeric
	PUSH	ECX								; store loop count
	LODSB									; puts first digit in AL

	; check if input is between 0 and 9
	CMP		AL, 48
	JB		_error
	CMP		AL, 57
	JA		_error

	; use numInt = 10*numInt + (numChar - 48) to convert ACSII to numbers
	CLC
	MOVZX	EDI, AL							; copy value of AL to EDI and zero extend = numChar
	MOV		EAX, [EBX]						; numInt
	MOV		ECX, 10
	MOV		EDX, 0
	MUL		ECX								; multiply by 10
	POP		ECX								; restore loop counter
	CMP		EDX, 0							
	JNZ		_error							; check if overflow and bigger than 32 bit signed
	SUB		EDI, 48							; (numChar-48) 
	ADD		EAX, EDI
	JC		_error							; if carry is set, then the addition caused it to exceed max value
	MOV		[EBX], EAX						; set numInt to new value
	CMP		EAX, registerUpperLimit			; check if larger than > 2147483647	
	JA		_edgeCase						; check if it's an edge case
	LOOP	_convertToNumLoop
	JMP		_checkNegative

_edgeCase:
	; handle edge case where value is 2147483647 or -2147483648	
	MOV		ECX, [EBP + 20]
	MOV		EDX, [ECX]
	CMP		EDX, 0
	JE		_error							; if positive and > upperLimit, then error
	CMP		EAX, registerLowerLimit
	JLE		_isNegative						; check for edge case of exactly -2147483648
	JMP		_error
	

_checkNegative:
	; checks if the value needs to be converted back to negative
	MOV		EAX, [EBP + 20]
	MOV		EDX, [EAX]
	CMP		EDX, 1							; if positive, skip to addToArray
	JNE		_addToArray

_isNegative:
	; sets the value to negative before storing the array
	MOV		EAX, [EBX]
	NEG		EAX
	CMP		EAX, registerLowerLimit			; if less than -2147483648, it does not fit in the register
	JL		_error
	MOV		[EBX], EAX
	JMP		_addToArray

_error:
	; displays an error when the input is not a 32 bit signed int or is too large/small
	MOV		EDX, [EBP + 28]					; write error message
	CALL	writeString
	MOV		EDX, [EBP + 20]
	MOV		EBX, 0
	MOV		[EDX], EBX						; reset value of isNegative
	MOV		EDX, [EBP + 24]
	MOV		[EDX], EBX						; reset value of userInputNumeric
	JMP		_getString						; reprompt user for a valid input
	
_addToArray:
	; adds the value in EBX to the array
	MOV		EAX, [EBP + 36]					
	MOV		ECX, [EAX]						; move value in integerCount to ECX
	MOV		ESI, [EBP + 32]					; move signedArray into ESI
	MOV		EDX, [EBX]						; numeric value of int into EDX
	MOV		[ESI + ECX], EDX				; move value into array, use integerCount to track proper index
	ADD		ECX, 4							; move to next index
	MOV		[EAX], ECX						; update integerCount

_restoreStack:
	; resets isNegative, userInputNumeric, and restores registers/stack						
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
