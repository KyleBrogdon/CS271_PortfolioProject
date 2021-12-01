TITLE CS271 Project 6: Low level I/O and macros     (Proj6_BrogdonK.asm)

; Author: Kyle Brogdon
; Last Modified: 26NOV2021
; OSU email address: brogdonK@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 06DEC2021
; Description: This program introduces the program and the author to the user, displays the rules to the user,
;				then prompts user for NUMINTS of signed 32 bit integers.  Each integer from user is captured as a string,
;				converted from ASCII to an integer, validated as a proper 32bit signed int, stored in an array, and then 
;				displayed back to the user by converting it back to ASCII and printing the string.  The program also 
;				displays the sum and floor rounded average of NUMINTS integers. Program then says goodbye to user.
;					**EC 1**: Each line of valid input for the user is numbered, and a running total is displayed using
;						writeVAL
;             

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

;-----------------------------------------------------------------------------------------------
; Name: mDisplayString
;
; Displays a string to the user
;
; Preconditions: Irvine32 must be included, string must be passed by reference
;
; Receives:
;				string				= Address of a string to display
;-----------------------------------------------------------------------------------------------
mDisplayString MACRO string: REQ

	PUSH	EDX
	MOV		EDX, string
	CALL	writeString
	POP		EDX

ENDM

NUMINTS = 10											; number of ints we must collect from the user
MAXSIZE = 100											; max size of the user's accepted input, much larger than max char for 32 bit register
registerUpperLimit = 2147483647							; max value that can be accepted into a signed 32 bit register
registerLowerLimit = 2147483648							; (negated) minimum value that can be accepted into a signed 32 bit register

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
	extraCredit1		BYTE		"**EC 1: Numbers each line of user input and displays running subtotal of the user's valid numbers using writeVal**", 13, 10, 0
	userInputPrompt		BYTE		". Please enter a signed number: ", 0
	errorPrompt			BYTE		"ERROR: You did not enter a signed number or the value was too large. Please try again.", 13, 10, 0
	numbersInputString	BYTE		"You entered the following numbers: ", 13, 10, 0
	sumString			BYTE		"The sum of these numbers is: " , 0
	roundedString		BYTE		"The rounded average is: ", 0
	runningTotalString	BYTE		"The running total of your signed ints is: ", 0
	farewell			BYTE		"Goodbye, and thanks for using this program!", 13, 10, 0
	userInputNumeric	SDWORD		?					; output variable for numeric conversion of user's input string
	commaSpace			BYTE		", ", 0				; used to print commas between values when they are printed back to user
	nullTerm			BYTE		" ",13,10,0			; used to print next line via macro
	stringLen			DWORD		?					; storage variable for the length of the user's input string
	isNegative			DWORD		0					; flag to track if input value is negative
	integerCount		DWORD		0					; keeps track of number of integers in the signedArray to properly index
	runningTotal		SDWORD		0					; used to tally running total of sum of numbers, and to display sum of array
	userLines			DWORD		1					; numbers user lines after valid input

.code

;-----------------------------------------------------------------------------------------------
; Name: main
;
; Calls introduction to introduce the program, author, and rules to the user.  Then loops NUMINTS
;	amount of times calling readVal to acquire string inputs of 32bit signed ints, convert to int, 
;	and validate a 32bit integer as a string from the user, then stores the value in an SDWORD 
;	array The program keeps a running tally of the sum of all currently collected signed ints 
;	while readVal is being looped and displays this to the user using writeVal. Once all ints 
;	have been collected, the program displays a list of the signed ints entered by the user by 
;	converting them from ASCII back to signed int via writeVal. Program then says goodbye to user.
;
; Preconditions: Irvine32 must be included, NUMINTS must be declared, and .data must contain all arrays/strings
;					used by subprocedures. String arrays must be type BYTE, signedArray must be DWORD.
;
; Postconditions: All DUP arrays have values changed, userInputNUmeric, stringLen, integerCount, runningTotal,
;					and userLines have their values changed.
;
; Returns: none
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
	; Prompts user for NUMINTS signed integers, then converts and stores in userInputNumeric
	PUSH	OFFSET numLinesArray						; stores converted userLines
	PUSH	OFFSET userLines							; counts number of user lines on input
	PUSH	OFFSET signedArray							
	PUSH	OFFSET errorPrompt
	PUSH	OFFSET userInputNumeric						; temporary storage for converted string
	PUSH	OFFSET isNegative							; tracks if value is negative
	PUSH	OFFSET stringLen							; length of user input
	PUSH	OFFSET userInputString						; store user's input
	PUSH	OFFSET userInputPrompt						; used by mGetString to prompt user
	CALL	readVal

	; add value converted into userInputNumeric to signedArray
	MOV		EAX, userInputNumeric					
	MOV		ESI, OFFSET signedArray						; move value in integerCount to ECX
	MOV		EBX, integerCount
	MOV		[ESI + EBX], EAX							; move value into array, use integerCount to track proper index
	ADD		integerCount, 4								; move to next index
	INC		userLines

	mDisplayString OFFSET runningTotalString			; print runningTotal title

	MOV		EAX, userInputNumeric

	; updates the running total and resets the userInputNumeric variable
	ADD		runningTotal, EAX
	PUSH	OFFSET runningConverted
	PUSH	runningTotal
	CALL	writeVal									; print the convertedRunningTotal
	MOV		userInputNumeric, 0							; reset value of userInputNumeric
	DEC		ECX											; decrease loop counter
	CALL	CrLF

	; clear runningConverted array for next input
	PUSH	ECX											; preserve loop counter
	CLD
	MOV		EAX, 0
	MOV		ECX, SIZEOF runningConverted
	MOV		EDI, OFFSET runningConverted
	REP		STOSB
	POP		ECX											; restore loop counter
	
	CMP		ECX, 0
	JNZ		_getIntsLoop								; continue loop until 0
	mDisplayString OFFSET nullTerm
	
	; set up loop for display signedArray values
	MOV		ECX, NUMINTS
	MOV		EBX, 0										; used to track correct index

	mDisplayString OFFSET numbersInputString			; print numbers entered title

_displayNumbersInputed:
	; displays each number the user input as a string separated by commas
	MOV		ESI, OFFSET signedArray
	ADD		ESI, EBX
	MOV		EDX, [ESI]									; register indirect access to SDWORD 
	PUSH	OFFSET userOutputString						; storage string for all DWORD values
	PUSH	EDX
	CALL	writeVal									; display current DWORD using writeVal
	CMP		ECX, 1										; check if we're on the last value of the array
	JE		_lastVal
	mDisplayString OFFSET commaSpace					; print comma and space after each value
	ADD		EBX, 4										; move to next index

	; clear userOutputString array
	PUSH	ECX											; store loop counter
	CLD
	MOV		EAX, 0
	MOV		ECX, SIZEOF userOutputString
	MOV		EDI, OFFSET userOutputString
	REP		STOSB
	POP		ECX											; restore loop counter

	LOOP	_displayNumbersInputed

_lastVal:
	; last value will not need a comma
	mDisplayString OFFSET nullTerm
	CALL	Crlf

	; convert the sum of all signed int input back to string, then display it
	mDisplayString OFFSET sumString						; print sum of strings title
	PUSH	OFFSET arraySumConverted					; storage string for the converted sum
	PUSH	runningTotal
	CALL	writeVal									; use writeVal to convert and display sum of the numbers
	CALL	Crlf
	mDisplayString OFFSET nullTerm	

	mDisplayString OFFSET roundedString					; print rounded average title

	; Calculates the average of the input ints
	MOV		EAX, runningTotal
	CDQ
	MOV		EBX, NUMINTS
	IDIV	EBX
	PUSH	OFFSET averageConverted						; storage string for the converted average
	PUSH	EAX											; push floor average to writeVal for conversion and display
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
;	for signed 32bit. Once it is validated, it is stored in an output variable userInputNumeric.
;		**EC 1** : readVal also calls writeVal in order to properly number lines of userInput
;
; Preconditions: Irvine32 must be included, registerUpperLimit and registerLowerLimit must be declared, 9 DWORD length
;					arguments must be passed via the stack.
;
; Postconditions: Values of userInputString, stringLen, signedArray, userLines, and numLinesArray are changed.
;
; Receives:
;				[EBP + 40]			= numLinesArray by reference
;				[EBP + 36]			= userLines by reference
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
	; acquire a string from the user
	MOV		ESI, [EBP + 36]						
	MOV		EAX, [ESI]									; userLines into ESI
	MOV		EDX, [EBP + 40]								; storage array into EDX for numLines
	PUSH	EDX	
	PUSH	EAX
	CALL	writeVal									; number the lines

	; invoke mGetstring and get user input as a string
	mGetString [EBP + 8], [EBP + 12], [EBP + 16]

	; check if user entered anything
	CLD	
	MOV		ECX, [EBP + 16]								; stringLen into ECX
	MOV		EBX, 0
	CMP		[ECX], EBX
	JE		_noEntry										; means the user provided no input

	; check if a sign was entered
	MOV		ESI, [EBP + 12]								; move string of numerical digits into ESI
	LODSB
	CMP		AL, 43										; check for plus sign
	JE		_leadingPlusSign
	CMP		AL, 45										; check for minus sign
	JE		_setNegative
	MOV		ESI, [EBP + 12]								; reset ESI because there is no sign in front of the string
	JMP		_loopSetup
	

_setNegative:
	; sets isNegative flag and sets up loop
	MOV		EDX, [EBP + 20]
	MOV		EBX, 1							
	MOV		[EDX], EBX									; set isNegative to 1
	MOV		EDI, [ECX]
	DEC		EDI											; decrease loop counter so stringLen does not count the sign
	MOV		ECX, EDI
	MOV		EBX, [EBP + 24]								; move userInputNumeric to EBX to store value once converted
	JMP		_convertToNumLoop

_leadingPlusSign:
	; handles where a + sign was put in front of the input
	MOV		EDI, [ECX]
	DEC		EDI
	MOV		ECX, EDI									; decrease loop counter so stringLen does not count the sign
	MOV		EBX, [EBP + 24]								; move userInputNumeric to EBX to store value once converted
	JMP		_convertToNumLoop

_loopSetup:
	; sets up userInputNumeric and loop counter
	MOV		EBX, [EBP + 24]
	MOV		EDI, [ECX]
	MOV		ECX, EDI

_convertToNumLoop:
	; converts string inputs to numeric
	PUSH	ECX											; store loop count
	LODSB												; puts first digit in AL

	; check if input is between 0 and 9
	CMP		AL, 48
	JB		_error
	CMP		AL, 57
	JA		_error

	; use numInt = 10*numInt + (numChar - 48) to convert ACSII to numbers
	CLC
	MOVZX	EDI, AL										; copy value of AL to EDI and zero extend = numChar
	MOV		EAX, [EBX]									; numInt
	MOV		ECX, 10
	MOV		EDX, 0
	MUL		ECX											; multiply by 10
	POP		ECX											; restore loop counter
	CMP		EDX, 0							
	JNZ		_afterPopError								; check if overflow and bigger than 32 bit signed
	SUB		EDI, 48										; (numChar-48) 
	ADD		EAX, EDI
	JC		_AfterPopError								; if carry is set, then the addition caused it to exceed max value
	MOV		[EBX], EAX									; set numInt to new value
	CMP		EAX, registerUpperLimit						; check if larger than > 2147483647	
	JA		_edgeCase									; check if it's an edge case
	LOOP	_convertToNumLoop
	JMP		_checkNegative

_edgeCase:
	; handle edge case where value is 2147483647 or -2147483648	
	MOV		ECX, [EBP + 20]
	MOV		EDX, [ECX]
	CMP		EDX, 0
	JE		_afterPopError								; if positive and > upperLimit, then error
	CMP		EAX, registerLowerLimit
	JLE		_isNegative									; check for edge case of exactly -2147483648
	JMP		_afterPopError
	

_checkNegative:
	; checks if the value needs to be converted back to negative
	MOV		EAX, [EBP + 20]
	MOV		EDX, [EAX]
	CMP		EDX, 1										; if positive, jump to restoreStack since value is in userInputNumeric
	JNE		_restoreStack

_isNegative:
	; sets the value to negative before storing the array
	MOV		EAX, [EBX]
	NEG		EAX
	CMP		EAX, registerLowerLimit						; if less than -2147483648, it does not fit in the register
	JL		_error
	MOV		[EBX], EAX
	JMP		_restoreStack

_error:
	; displays an error when the input is not a 32 bit signed int or is too large/small
	POP		ECX
	MOV		EDX, [EBP + 28]								; write error message
	CALL	writeString
	MOV		EDX, [EBP + 20]
	MOV		EBX, 0
	MOV		[EDX], EBX									; reset value of isNegative
	MOV		EDX, [EBP + 24]
	MOV		[EDX], EBX									; reset value of userInputNumeric
	JMP		_getString									; reprompt user for a valid input

_afterPopError:
	; displays an error but handles when the loop has already popped the value of ECX to maintain stack alignment
	MOV		EDX, [EBP + 28]								; write error message
	CALL	writeString
	MOV		EDX, [EBP + 20]
	MOV		EBX, 0
	MOV		[EDX], EBX									; reset value of isNegative
	MOV		EDX, [EBP + 24]
	MOV		[EDX], EBX									; reset value of userInputNumeric
	JMP		_getString									; reprompt user for a valid input

_restoreStack:
	; resets isNegative and restores registers/stack						
	MOV		EDX, [EBP + 20]
	MOV		EBX, 0
	MOV		[EDX], EBX									; reset value of isNegative
	POPAD
	POP		EBP
	RET		36

_noEntry:
	;no valid entry from user
	MOV		EDX, [EBP + 28]								; write error message
	CALL	writeString
	JMP		_getString

readVal ENDP

;-----------------------------------------------------------------------------------------------
; Name: writeVal
;
; Takes a signed integer and an output string array, converts the signed integer from DWORD to ASCII,
;	adds it to the output string array, then invokes mDisplayString to print the array.
;
; Preconditions: irvine32 must be included, array must be type BYTE or SBYTE and passed by reference,
;					integer must be passed by value.
;
; Postconditions: output string array is changed.
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

	; set up loop
	MOV		EDI, [EBP + 12]								; outputString to EDI
	CLD
	MOV		ECX, 0										; number of digits counter
	MOV		EAX, [EBP + 8]								; signed int to EAX
	CMP		EAX, 0										; check if signed int is negative
	JGE		_convertToDigits

_setNegative:	
	NEG		EAX											; two's complement if negative
	PUSH	EAX
	MOV		AL, 45										; add negative sign
	STOSB
	POP		EAX

_convertToDigits:
	; Converts SDWORD values to single digits for conversion to ASCII
	MOV		EBX, 10
	MOV		EDX, 0
	DIV		EBX
	PUSH	DX											; push this digit of the int to the stack (from right side to left)
	INC		ECX											; increment number of digits
	CMP		EAX, 0
	JNZ		_convertToDigits							; continue until EAX is 0


_convertToASCII:
	; converts each single digit to ASCII, and stores as a string separated by commas
	POP		AX											; retreives from the stack in the correct order
	ADD		AL, 48										; converts to ASCII
	STOSB												; puts in output string array
	LOOP	_convertToASCII


	mDisplayString [EBP + 12]							; prints integer as a string

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
