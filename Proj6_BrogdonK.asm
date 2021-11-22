TITLE CS271 Project 6: Low level I/O and macros     (Proj6_BrogdonK.asm)

; Author: Kyle Brogdon
; Last Modified: 21NOV2021
; OSU email address: ONID_ID@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 06DEC2021
; Description: This file is provided as a template from which you may work
;              when developing assembly projects in CS271.

INCLUDE Irvine32.inc

mGetString MACRO
	; Display a prompt (input parameter, by reference), then get the user�s keyboard input into a memory location (output parameter, by reference). 
	; You may also need to provide a count (input parameter, by value) for the length of input string you can accommodate and a provide a number 
	; of bytes read (output parameter, by reference) by the macro.
ENDM

mDisplayString MACRO
	; Print the string which is stored in a specified memory location (input parameter, by reference).
ENDM

NUMINTS = 10								; number of integers we must collect from the user

.data
	signedArray		SDWORD		NUMINTS DUP (?)
	introProgram1	BYTE		"Computer Architecture and Assembly Project 6: Low level input/output procedures and macros", 0
	introProgram2	BYTE		"Written by: Kyle Brogdon", 13, 10, 0
    programRules1	BYTE		"Please enter ", 0
	programRules2	BYTE		" signed decimal integers.", 0
	programRules3	BYTE		"Each user number must fit inside a 32 bit register.", 0
	programRules4	BYTE		"After you have finished entering the signed numbers, I will display a list of the integers, their sum, and average.", 13, 10, 0
	userInputString	BYTE		"Please enter a signed number: ", 0
	errorPrompt		BYTE		"ERROR: You did not enter a signed number or the value was too large. Please try again.", 13, 10, 0
	farewell		BYTE		"Goodbye, and thanks for using this program!", 13, 10, 0

.code

;-----------------------------------------------------------------------------------------------
; Name: main
;
; Generates ARRAYSIZE of random integers from LO to HI and then displays the list of random ints to user,
;		sorts the list, displays the sorted list, displays the median value of the list in ascending order,
;		then displays the count of each occurence of each random int in the list using the stack for parameter
;		passing. No global variables used except CONSTANTS.
;
; Preconditions: Irvine32 must be included, NUMINTS must be declared, and .data must contain all arrays/strings
;										used by subprocedures. Array must be type SDWORD.
;
;-----------------------------------------------------------------------------------------------

main PROC
	; introduces the user to the program, the programmer, and the rules
	PUSH	OFFSET programRules4
	PUSH	OFFSET programRules3
	PUSH	OFFSET programRules2
	PUSH	OFFSET programRules1
	PUSH	OFFSET introprogram2
	PUSH	OFFSET introProgram1
	CALL	introduction

	MOV		ECX, NUMINTS
_mainLoop:

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
;				[EBP + 28]			= programRules4 passed by reference
;				[EBP + 24]			= programRules3 passed by reference
;				[EBP + 20]			= programRules2 passed by reference
;				[EBP + 16]			= programRules1 passed by reference
;				[EBP + 12]			= introProgram2 passed by reference
;				[EBP + 8]			= introProgram1 passed by reference
;				[EBP + 4]
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
	
	;print programRules4
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
;
; Preconditions: 
;
; Postconditions: 
;
; Receives:
;				[EBP + 20]			= 
;				[EBP + 16]			= 
;				[EBP + 12]			= 
;				[EBP + 8]			= 
;
; Returns: None	
;-----------------------------------------------------------------------------------------------

readVal PROC

RET
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
;				[EBP + 16]			= 
;				[EBP + 12]			= 
;				[EBP + 8]			= 
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
