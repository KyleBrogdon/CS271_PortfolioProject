TITLE CS271 Project 6: Low level I/O and macros     (Proj6_BrogdonK.asm)

; Author: Kyle Brogdon
; Last Modified: 26NOV2021
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

mGetString MACRO prompt:REQ, stringStorage:REQ, bytesRead:REQ, numberValidLines
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

mDisplayString MACRO string: REQ
	PUSH	EDX
	MOV		EDX, string
	CALL	writeString
	POP		EDX
ENDM

NUMINTS = 10										; number of ints we must collect from the user
MAXSIZE = 100										; max number of characters that can fit in a 32 bit register, including a leading sign
registerUpperLimit = 2147483647						; max value that can be accepted into a signed 32 bit register
registerLowerLimit = 2147483648						; (negated) minimum value that can be accepted into a signed 32 bit register

.data
	signedArray			SDWORD		NUMINTS DUP (?)
	userInputString		SBYTE		MAXSIZE DUP (?)
	userOutputString	SBYTE		MAXSIZE DUP (?)
	arraySumConverted	SBYTE		MAXSIZE DUP (?)
	averageConverted	SBYTE		MAXSIZE DUP (?)
	numLinesArray		SBYTE		MAXSIZE DUP (?)
	runningConverted	SBYTE		MAXSIZE DUP (?)
	introProgram1		BYTE		"Computer Architecture and Assembly Project 6: Low level input/output procedures and macros", 0
	introProgram2		BYTE		"Written by: Kyle Brogdon", 13, 10, 0
    programRules1		BYTE		"Please enter ", 0
	programRules2		BYTE		" signed decimal integers. Each number must fit inside a 32 bit register.", 0
	programRules3		BYTE		"After you have finished entering the signed numbers, I will display a list of the integers, their sum, and average.", 0
	extraCredit1		BYTE		"**EC 2**: Numbers each line of user input and displays running subtotal of the user's valid numbers using writeVal", 13, 10, 0
	userInputPrompt		BYTE		". Please enter a signed number: ", 0
	userInputNumeric	SDWORD		?
	errorPrompt			BYTE		"ERROR: You did not enter a signed number or the value was too large. Please try again.", 13, 10, 0
	numbersInputString	BYTE		"You entered the following numbers: ", 13, 10, 0
	sumString			BYTE		"The sum of these numbers is: " , 0
	roundedString		BYTE		"The rounded average is: ", 0
	runningTotalString	BYTE		"The running total of your signed ints is: ", 0
	farewell			BYTE		"Goodbye, and thanks for using this program!", 13, 10, 0
	commaSpace			BYTE		", ", 0
	nullTerm			BYTE		" ",13,10,0
	stringLen			DWORD		?
	isNegative			DWORD		0
	integerCount		DWORD		0											; keeps track of number of integers in the signedArray in increments of 4 for DWORD
	arraySum			SDWORD		0
	runningTotal		SDWORD		0
	userLines			DWORD		1

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
	PUSHAD
	; introduces the user to the program, the programmer, and the rules
	PUSH	OFFSET extraCredit1
	PUSH	OFFSET programRules3
	PUSH	OFFSET programRules2
	PUSH	OFFSET programRules1
	PUSH	OFFSET introprogram2
	PUSH	OFFSET introProgram1
	CALL	introduction

	MOV		ECX, NUMINTS
_getIntsLoop:
	PUSH	OFFSET numLinesArray
	PUSH	OFFSET userLines
	PUSH	OFFSET signedArray
	PUSH	OFFSET errorPrompt
	PUSH	OFFSET userInputNumeric
	PUSH	OFFSET isNegative
	PUSH	OFFSET stringLen
	PUSH	OFFSET userInputString
	PUSH	OFFSET userInputPrompt
	CALL	readVal

	; add value from readVal to array
	MOV		EAX, userInputNumeric					
	MOV		ESI, OFFSET signedArray				; move value in integerCount to ECX
	MOV		EBX, integerCount
	MOV		[ESI + EBX], EAX					; move value into array, use integerCount to track proper index
	ADD		integerCount, 4						; move to next index
	INC		userLines

	mDisplayString OFFSET runningTotalString	; print runningTotal title
	MOV		EAX, userInputNumeric

	_continueRunning:
	ADD		runningTotal, EAX
	PUSH	OFFSET runningConverted
	PUSH	runningTotal
	CALL	writeVal							; print the convertedRunningTotal
	MOV		userInputNumeric, 0					; reset value of userInputNumeric
	DEC		ECX
	CALL	CrLF

	; clear runningConverted array
	PUSH	ECX
	CLD
	MOV		EAX, 0
	MOV		ECX, SIZEOF runningConverted
	MOV		EDI, OFFSET runningConverted
	REP		STOSB
	POP		ECX
	
	CMP		ECX, 0
	JNZ		_getIntsLoop
	mDisplayString OFFSET nullTerm
	
	; converts numbers input to an ASCII string and displays them
	MOV		ECX, NUMINTS
	MOV		EBX, 0								; used to track correct index
	mDisplayString OFFSET numbersInputString
_displayNumbersInputed:
	; displays each number the user input as a string separated by commas
	MOV		ESI, OFFSET signedArray
	MOV		EDX, [ESI + EBX]
	PUSH	OFFSET userOutputString
	PUSH	EDX
	CALL	writeVal
	CMP		ECX, 1
	JE		_lastVal
	mDisplayString OFFSET commaSpace
	ADD		EBX, 4

	; clear userOutputString array
	PUSH	ECX
	CLD
	MOV		EAX, 0
	MOV		ECX, SIZEOF userOutputString
	MOV		EDI, OFFSET userOutputString
	REP		STOSB
	POP		ECX

	LOOP	_displayNumbersInputed
_lastVal:
	mDisplayString OFFSET nullTerm

	; converts the sum of all signed ints input, then display it
	CALL	Crlf
	mDisplayString OFFSET sumString
	PUSH	OFFSET arraySumConverted
	PUSH	runningTotal
	CALL	writeVal
	CALL	Crlf
	mDisplayString OFFSET nullTerm

	mDisplayString OFFSET roundedString
	; Calculates the average of the input ints
	MOV		EAX, runningTotal
	MOV		EBX, NUMINTS
	MOV		EDX, 0
	DIV		EBX
	PUSH	OFFSET averageConverted
	PUSH	EAX					; push floor average to writeVal
	CALL	writeVal
	CALL	crlf

_sayGoodbye:
	; says farewell to the user
	PUSH	OFFSET farewell						
	CALL	goodbye
	POPAD								

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
;				[EBP + 28]			= extraCredit1 passed by reference
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

	MOV		EDX, [EBP + 28]
	CALL	writeString
	CALL	CrLF

	; restore stack
	POP		EDX
	POP		EAX
	POP		EBP
	RET		24
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
;				[EBP + 40]			= numLinesArray by reference
;				[EBP + 36]			= userLines by value
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

	MOV		ESI, [EBP + 36]
	MOV		EAX, [ESI]
	MOV		EDX, [EBP + 40]
	PUSH	EDX	
	PUSH	EAX
	CALL	writeVal

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
	JNE		_restoreStack

_isNegative:
	; sets the value to negative before storing the array
	MOV		EAX, [EBX]
	NEG		EAX
	CMP		EAX, registerLowerLimit			; if less than -2147483648, it does not fit in the register
	JL		_error
	MOV		[EBX], EAX
	JMP		_restoreStack

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
	

_restoreStack:
	; resets isNegative and restores registers/stack						
	MOV		EDX, [EBP + 20]
	MOV		EBX, 0
	MOV		[EDX], EBX						; reset value of isNegative
	POPAD
	POP		EBP
	RET		36

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
;				[EBP + 12]			= an array to store the output string by reference
;				[EBP + 8]			= a signed integer passed by value
;
; Returns: None	
;-----------------------------------------------------------------------------------------------

writeVal PROC
	PUSH	EBP
	MOV		EBP, ESP
	PUSHAD

	MOV		EDI, [EBP + 12]				; outputString to EDI
	CLD
	MOV		ECX, 0						; number of digits counter
	MOV		EAX, [EBP + 8]				;
	CMP		EAX, 0
	JGE		_convertToDigits

_setNegative:	
	NEG		EAX
	PUSH	EAX
	MOV		AL, 45						; add negative sign
	STOSB
	POP		EAX

_convertToDigits:
	; Converts SDWORD values to single digits for conversion to ASCII
	MOV		EBX, 10
	MOV		EDX, 0
	DIV		EBX
	PUSH	DX							; push this digit of the int to the stack (from right side to left)
	INC		ECX							; increment number of digits
	CMP		EAX, 0
	JNZ		_convertToDigits			; continue until EAX is 0


_convertToASCII:
	; converts each single digit to ASCII, and stores as a string separated by commas
	POP		AX
	ADD		AL, 48						; converts to ASCII
	STOSB
	LOOP	_convertToASCII


	mDisplayString [EBP + 12]

	POPAD
	POP		EBP
	RET		8
	writeVal ENDP

;-----------------------------------------------------------------------------------------------
; Name: goodbye
;
; Takes a string, then says goodbye and thank you to the user.
;
; Preconditions: Irvine32 must be included.
;
; Receives:
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
