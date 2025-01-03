;*******************************************************
;* CMPEN 472, Homework 12: Data Monitor Program
;* CodeWarrior Simulator/Debug edition, not for CSM-12C128 board
;*
;* Dec, 6, 2024 Kevin McKnight
;* 
;* This program is a data management and monitor program
;* you are able to view what data is in memory using the show
;* and MD commands and you are able to save new data using the
;* write and LD commands. You can load or view one byte at a time
;* or many bytes at once depending on which command is used.
;* The LD and MD will load many at once and the show and write
;* command will only load one. 
;*******************************************************

; export symbols - program starting point
            XDEF        Entry        ; export 'Entry' symbol
            ABSENTRY    Entry        ; for assembly entry point

; include derivative specific macros
PORTA       EQU         $0000
PORTB       EQU         $0001
DDRA        EQU         $0002
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section for clock
            ORG     $3000             ; RAMStart defined as $3000

ccount      DS.B    1
flag        DS.B    1 
userin      DS.B    2
temp        DS.B    2
userwrt     DS.B    2
decwrt      DS.B    2
CTR         DS.B    1
BUF         DS.B    6
gotMem      DS.B    2
addyct      DS.B    1
numlen      DS.B    1
bytecounter DS.B    2
linecount   DS.B    1
twocount    DS.B    1
whennextln  DS.B    1
LDBuff      DS.B    2


;*******************************************************
; code section

            ORG     $3100
Entry
            LDS     #Entry         ; initialize the stack pointer

            LDAA    #%11111111   ; Set PORTA and PORTB bit 0,1,2,3,4,5,6,7
            STAA    DDRA         ; all bits of PORTA as output
            STAA    PORTA        ; set all bits of PORTA, initialize
            STAA    DDRB         ; all bits of PORTB as output
            LDAA    #%00000000
            STAA    PORTB        ; set all bits of PORTB, initialize
                      
            ldaa    #$0C         ; Enable SCI port Tx and Rx units
            staa    SCICR2       ; disable SCI interrupts

            ldd     #$0001       ; Set SCI Baud Register = $0001 => 1.5M baud at 24MHz (for simulation)
            std     SCIBDH       ; SCI port baud rate change
            
            
            ldy     #msgStart
            jsr     printmsg
            jsr     nextline

restartLoop
            clr     ccount          ;reset everything
            clr     flag
            clr     numlen
            clr     whennextln
            clr     linecount
            ldx     #$0000
            stx     userin
            stx     decwrt
            stx     gotMem
            stx     bytecounter
            stx     userwrt
            stx     LDBuff
            
            
            ldx     #Buff           ;load in buff for userinput
            
            ldaa    #$3E
            jsr     putchar

loop        
            jsr     getchar          ; type writer - check the key board
            tsta                    ;  if nothing typed, keep checking
            beq     loop
            
            staa    1,X+
            inc     ccount
            
            cmpa    #CR
            beq     enterPress       ; if enter is pressed go to check command
            jsr     putchar          ; otherwise display char on the terminal window
            bne     loop
            
enterPress 
            
            ldx     #Buff            ; load Buff into x reg 
            pshx                    ; save x reg
            jsr     checkCommand     ; go to check what command user entered
            bra     restartLoop            
          
          
          

;***************checkCommand*******************
;* Program: This will check the users input to see what we need to do
;* Input: user input Buff   
;* Output: no output unless error but will send us to proper subroutine  
;* Registers modified: B, Y, A
;**********************************************
checkCommand
            
            ldy     #Buff
            ldaa    0,Y
            
            cmpa    #$53             ; check to see if S
            lbeq    show
            
            cmpa    #$57             ; check to see if W
            lbeq    write
            
            cmpa    #$4D             ; check to see if M
            lbeq    display
            
            cmpa    #$4C             ; check to see if L
            lbeq    load
            
            cmpa    #$47             ; check to see if G
            lbeq    go
            
            cmpa    #$51             ; check to see if Q
            lbeq    quit
            
            lbra    errorMsg        ; if none of those inputs go to error message          
          
 
 
 
;***********show*******************************
;* Program: Will show the memory in a user specified address
;* Input: user inputted address  
;* Output: the memory in that address 
;* Registers modified: D, X, Y
;**********************************************


show        
            ldab    #$07                   ;load in 7 since thats the largest S command we can have
            cmpb    ccount                 ;check to make sure it is that long
            lbne    errorMsg
            
            ldy     #Buff                  ;load in the buffer into y
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$24                   ;check to make sure its a $ since all addresses should be in hex
            lbne    errorMsg
            ldx     #$0000                 ;clear x since we use it later to store user input    
            jsr     gotoaddress 
            stx     userin           
            
            
showmem                
            jsr     nextline              ;go to next lin
            
            ldaa    #$24                  ;load in $
            jsr     putchar
            
            ldd     userin                ;print what the user just entered (ie $3000)
            jsr     printHx               ;printHx prints one byte
            tba
            jsr     printHx
            
            ldaa    #$3D                  ;put = 
            jsr     putchar
            ldaa    #$3E                  ;put >
            jsr     putchar
            ldaa    #$25                  ;put %
            jsr     putchar
            
            ldaa    X                     ;print in binary first
            jsr     printbinary
            
            inx                           ;print the lower byte
            ldaa    X
            jsr     printbinary
               
            ldy     #tabmsg               ;tab over once for formatting
            jsr     printmsg
            ldaa    #$24                  ;put $
            jsr     putchar
            dex
            
            ldaa    X                     ;print in hex
            clrb
            std     gotMem
            jsr     printHx
  
            inx
            ldaa    X                     ;print lower byte
            jsr     printHx
            
            ldab    X                     ;load in full num
            ldx     gotMem
            abx
            stx     gotMem
            
            ldy     #tabmsg
            jsr     printmsg
                
            ldd     gotMem                ;print in dec
            jsr     pnum10
            
            
            
            lbra    restartLoop
            
            

;***********write******************************
;* Program: Will write data to a user specified address
;* Input: Address, and data   
;* Output: Will change the data at said address 
;* Registers modified: Y,X,D
;**********************************************
write
            ldy     #Buff                  ;load in the buffer into y
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$24                   ;check to make sure its a $ since all addresses should be in hex
            lbne    errorMsg
            ldx     #$0000                 ;clear x since we use it later to store user input    
            jsr     gotoaddress            ;get user entered address
            stx     userin                 ;store that
            
getdata
            iny                            ;iny y twice to get the data we want to write
            iny
            ldab    0,Y                    ;check for $ to see if its a hex num or decimal
            cmpb    #$24
            bne     isdecnum
            ldx     #$0000                 ;if its hex we can store it without further manipulation
            jsr     gotoaddress
            stx     userwrt
            bra     writemem

              
isdecnum    
            dey                            ;if its a decimal number then we need to convert it to a hex number first
            ldx     #$0000
            jsr     gotoaddress
            stx     decwrt
            jsr     convertDec
            
   
writemem
            ldy     userwrt                ;load the data we want to write to memory in Y
            ldx     userin                 ;load the address we want to write to in X
            sty     0,X                    ;store Y in X
            
            lbra     showmem               ;print with the same formatting as Show 


;***********display****************************
;* Program: Will display a large block of data from
;* user specified address and amount
;* Input: address and amount of bytes to display   
;* Output: The data in the addresses specified 
;* Registers modified: X,Y,D
;**********************************************
display
            jsr     grabaddress          ;get address we want to go to
            
            ldd     userin               ;store address to d
            jsr     printHx              ;print adress in hex
            tfr     B,A
            jsr     printHx
            ldaa    #$20                 ;print space for formatting
            jsr     putchar
            
displaymem
            ldaa    X                    ;load whatever X is pointing to in A
            jsr     printHx              ;print that byte
            ldaa    #$20                 ;add a space for formatting
            jsr     putchar
            inx                          ;increase X
            inc     linecount            ;increase line count which we use to keep track if we need to start a new line or not
                       
            dey                          ;decrease y which has the amount of bytes we need to print in it
            cpy     #$00                 ;see if it has hit 0
            beq     donememdisp
            
            ldaa    #$10                 ;compare $10 against the amount of bytes we printed to see if we need a new line
            cmpa    linecount   
            beq     newmemline

            bra     displaymem           ;repeat process
            
newmemline            
            clr     linecount            ;reset the line counter
            jsr     nextline             ;print new line
            tfr     X,D                  ;print the address we are now at in hex
            jsr     printHx
            tfr     B,A
            jsr     printHx
            ldaa    #$20                 ;print space
            jsr     putchar
            bra     displaymem           ;repeat process

donememdisp 
            jsr     nextline
            lbra    restartLoop          ;finish 
            

;***********load*******************************
;* Program: Will load in data in continous memory locations
;* based off of user specified data and memory location
;* Input: memory location and amount of data to be stored, 
;* and all of the data to be stored  
;* Output: Will store specified data in memory 
;* Registers modified: X,Y,D
;**********************************************
load
           jsr      grabaddress
           ldaa     #$02
           staa     twocount
           ldaa     #$00
           staa     whennextln
           ldx      #LDBuff
           ldy      bytecounter           
           
loadloop        
           jsr      getchar                ; type writer - check the key board
           tsta                            ; if nothing typed, keep checking
           beq      loadloop
          
aftrNewln            
           staa     1,X+                   ;store one byte to LDBuff which is a temp buff that only holds two bytes
           dec      twocount               ;decrease two count to show we have one byte
           inc      whennextln
           
           cpy      #$00
           lbeq     memlim
           
           ldab     #$21
           cmpb     whennextln
           beq      enterPressMD           ; if enter is pressed go to check command
           
           cmpa     #$0D
           lbeq     errorMsg2
           
           jsr      putchar                ; otherwise display char on the terminal window
           
           tst      twocount               ;see if we have processed two bytes ie(34, 35 -> $45)
           beq      putbytemem             ;if we have we want to put that byte in memory
           
           bne      loadloop               ;if we haven't processes two bytes then go back to loop and process another

putbytemem
           dey
           ldx      #LDBuff                ;load the buff back in from the start
           ldab     X                      ;load byte1 into b
           jsr      testValid              ;make sure its a hex value
              
           ldaa     flag                   ;load in flag to see if it was a letter or number
           cmpa     #$01                   
           lbeq     letterLD
numberLD
           subb     #$30                   ;sub 30 if its number
           bra      nextnumLD
letterLD           
           subb     #$37                   ;sub 37 if its a letter           
nextnumLD
           clr      flag
           ldaa     #$10
           mul                             ;shift left 
           tfr      B,A                    ;transfer to A
           
           inx                             ;increase x to get the next byte
           ldab     X                      ;load in next byte
           jsr      testValid              ;see if its valid
           psha                            ;save A since we need to use it for something else
           ldaa     flag                   ;load in flag
           cmpa     #$01
           lbeq     letterLD2
numberLD2
           subb     #$30                   ;sub 30 if its number
           bra      nextnumLD2
letterLD2           
           subb     #$37                   ;sub 37 if its a letter           
nextnumLD2
           pula                            ;restore A to get our value back
           aba                             ;add second byte to the first one to get proper hex value
           
           ldx      userin                 ;load in where we want to store value
           staa     0,X                    ;store value to that mem location
           inx                             ;increase mem pointer to point to next location
           stx      userin                 ;store that back to userin
           ldx      #LDBuff                ;load back in LDBuff for next value
           ldaa     #$02                   ;restore twocount
           staa     twocount
           clr      flag
           bra      loadloop               ;go back to repeat
           
           
enterPressMD           
           cmpa     #$0D
           lbne     errorMsg2     
           
           jsr      nextline
           ldx      #LDBuff  
           ldaa     #$02
           staa     twocount
           ldaa     #$00
           staa     whennextln     
test2entr  
             
           jsr      getchar                ; type writer - check the key board
           tsta                            ; if nothing typed, keep checking
           beq      test2entr
           cmpa     #$0D
           lbne     aftrNewln
                      
finMD
           lbra     restartLoop
           
memlim
           jsr      nextline
           ldy      #memlimMsg
           jsr      printmsg
           jsr      nextline
           lbra     restartLoop
           

;***********go*********************************
;* Program: Will go to the user specified address
;* Input: An address in memory   
;* Output: will jump to said address 
;* Registers modified: B, Y, PC
;**********************************************
go 
            ldab    #$08
            cmpb    ccount
            lbne    errorMsg
            
            jsr     nextline
            ldy     #Buff                  ;load in the buffer into y
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$4F                   ;check to make sure its a $ since all addresses should be in hex
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$24                   ;check to make sure its a $ since all addresses should be in hex
            lbne    errorMsg
            ldx     #$0000                 ;clear x since we use it later to store user input    
            jsr     gotoaddress 
            stx     userin   
            
            jmp     X                      ;set PC to whatever value is in X which is what the user entered
                    

;***********quit*******************************
;* Program: Will exit the program and start typewriter
;* Input: No input   
;* Output: Will exit data program 
;* Registers modified: X,Y,D
;**********************************************
quit          
            ldy     #Buff                  ;load in the buffer into y
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$55                   ;check to make sure its a $ since all addresses should be in hex
            lbne    errorMsg
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$49                   ;check to make sure its a $ since all addresses should be in hex
            lbne    errorMsg
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$54                   ;check to make sure its a $ since all addresses should be in hex
            lbne    errorMsg

            jsr   nextline
            ldy   #msgQuit          ; load in quit message
            jsr   printmsg
            jsr   nextline
            ldy   #msgQuit2
            jsr   printmsg
            jsr   nextline

typeWriter
            jsr    getchar          ; type writer - check the key board
            tsta                    ;  if nothing typed, keep checking
            beq    typeWriter
            
            staa   1,X+
                                    ;  otherwise - what is typed on key board
            jsr    putchar          ; is displayed on the terminal window
            cmpa   #CR
            bne    typeWriter       ; if Enter/Return key is pressed, move the
            ldaa   #LF              ; cursor to next line    
            jsr    putchar
            bra    typeWriter


;***********grabaddress************************
;* Program: Will get the address the user inputed
;* Input: memory address   
;* Output: will put said address into data to use later 
;* Registers modified: Y, X, D
;**********************************************
grabaddress
            ldy     #Buff                  ;load in the buffer into y
            iny                            ;iny to go to next char            
            ldab    0,Y                    ;load b with char in buff
            cmpb    #$44                   ;check to make sure its D
            lbne    errorMsg
            iny
            ldab    0,Y
            cmpb    #$24
            lbne    errorMsg
            
            ldx     #$0000                 ;clear x since we use it later to store user input    
            jsr     gotoaddress
            stx     userin                 ;get whatever address the user entered
            ldab    #$00
            stab    linecount
            
            iny                            ;inc y
            iny
            ldab    0,Y
            cmpb    #$24                   ;make sure that char is a $
            lbne    errorMsg
            
            ldx     #$0000
            jsr     gotoaddress            ;jump to go to address subroutine
            stx     bytecounter
            ldx     userin    
            ldy     bytecounter
            jsr     nextline
            rts

;***********gotoaddress************************
;* Program: will convert user address to hex and put it in memory
;* Input: user entered address   
;* Output: address
;* Registers modified: Y, X, D
;**********************************************
gotoaddress             
          
            clr     addyct            
            
addressloop
            iny                            ;iny to go to next char
            ldab    0,Y                    ;load in char to b
            jsr     testValid              ;test to make sure it is a valid hex number
            inc     addyct
            ldaa    #$05
            cmpa    addyct
            lbeq    errorMsg3
            
            
            ldaa    flag                   ;load in flag to see if it was a letter or number
            cmpa    #$01                   
            lbeq    letter
number
            subb    #$30                   ;sub 30 if its number
            bra     nextnum
letter           
            subb    #$37                   ;sub 37 if its a letter
nextnum
            clr     flag                   ;clear the flag so we can use it again
            abx                            ;add hex number to x
            ldaa    1,Y                    ;load in the next char
            cmpa    #$0D                   ;make sure its not a enter to signify the end of the number
            beq     done
            cmpa    #$20
            beq     done                   ;do the same thing for SPACE
            
            stx     temp                   ;store what we have to user in
            pshy                           ;save y
            ldy     #$10                   ;multiply user in by $10 to shift left once
            ldd     temp
            emul
            std     temp                   ;store new number to userin
            ldx     temp                   ;load that back in and repeat process until number is done
            puly                           ;restore y
            bra     addressloop

done
            rts



;***********convertDec**************************
;* Program: converts a decimal number to hex
;* Input: 4 or less digit decimal number   
;* Output: that number in hex
;* Registers modified: D, X, Y
;**********************************************
convertDec
           pshd                            ;save D,X,Y
           pshx
           pshy
           
                                           
           ldy      #$0000                 ;load userwrt with 0 so its not garbage data
           sty      userwrt                
           ldab     addyct
           
           cmpb     #$04                   ;check to see if we have 4 digits
           bne      twodigi
           
           pshd                            
           ldd      decwrt
           ldx      #$1000                 ;get the most valuable digit
           idiv
           tfr      X,Y                    ;transfer X to Y 
           ldd      #$3E8                  ;load D with the proper value to get the hex value
           emul                            ;multiply
           std      userwrt                ;store that value to user wrt
           puld
           decb

           
twodigi        
           cmpb     #$03                  ;check to see if we have 3 digits
           bne      threedigi
           pshd
           ldd      decwrt
           anda     #%00001111            ;wipe away the digit we already processed
           ldx      #$100                 ;get the most valuable digit
           idiv
           tfr      X,Y                   ;same process as above but now we add number to what we already got
           ldd      #$64
           emul
           addd     userwrt
           std      userwrt 
           puld
           decb

           
threedigi           
           cmpb     #$02                 ;same thing as above but for the tens digit
           bne      fourdigi
           pshd
           ldd      decwrt
           anda     #%00000000
           ldx      #$10
           idiv
           tfr      X,Y
           ldd      #$0A
           emul
           addd     userwrt
           std      userwrt
           pulx
           dex
           
           

           
fourdigi   
           ldd      decwrt              ;add the ones digit last
           anda     #%00000000
           andb     #%00001111        
           addd     userwrt
           std      userwrt
           
doneconv    
           puld
           pulx
           puly       
           rts
           
            
  
;***********testValid**************************
;* Program: will make sure entered number is hex valid
;* Input: hex value   
;* Output: flag to tell if its a letter or number 
;* Registers modified: B
;**********************************************
testValid
            cmpb    #$30                   ;test to see if number is a valid hex number and if its a letter or number
            beq     valid
            cmpb    #$31
            beq     valid
            cmpb    #$32
            beq     valid
            cmpb    #$33
            beq     valid
            cmpb    #$34
            beq     valid
            cmpb    #$35
            beq     valid
            cmpb    #$36
            beq     valid
            cmpb    #$37
            beq     valid
            cmpb    #$38
            beq     valid
            cmpb    #$39
            beq     valid

            inc     flag
            cmpb    #$41
            beq     valid
            cmpb    #$42
            beq     valid
            cmpb    #$43
            beq     valid
            cmpb    #$44
            beq     valid
            cmpb    #$45
            beq     valid
            cmpb    #$46
            beq     valid
            
            
            clr     flag
            lbra    errorMsg
 
valid
           
            rts
 
;***********printbinary***********************
;
printbinary
            pshx                                   ;converts number to binary then prints
            psha
            
            lsra
            lsra
            lsra
            lsra
            
            pshd
            jsr    binaryTable
            jsr    putbin
            puld
            
            
            pula
            anda   #$0f
            pshd
            jsr    binaryTable
            jsr    putbin
            puld  
            
            pulx  
              
            rts

;*************putbin***************************
putbin
            psha
            lsra
            lsra
            lsra
            lsra
            
            adda    #$30
            jsr     putchar
            
            pula    
            anda    #$0f
            adda    #$30
            jsr     putchar
            
            tba
            psha
            lsra
            lsra
            lsra
            lsra
            
            adda    #$30
            jsr     putchar
            
            pula
            anda    #$0f
            adda    #$30
            jsr     putchar

            rts
            
;***********printHx***************************
; prinHx: print the content of accumulator A in Hex on SCI port
printHx     psha
            lsra
            lsra
            lsra
            lsra
            cmpa   #$09
            bhi    alpha1
            adda   #$30
            jsr    putchar
            bra    low4bits
alpha1      adda   #$37
            jsr    putchar            
low4bits    pula
            anda   #$0f
            cmpa   #$09
            bhi    alpha2
            adda   #$30
            jsr    putchar
            rts
alpha2      adda   #$37
            jsr    putchar
            rts
            
;***********pnum10***************************
;* Program: print a word (16bit) in decimal to SCI port
;* Input:   Register D contains a 16 bit number to print in decimal number
;* Output:  decimal number printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Keep divide number by 10 and keep the remainders
;     Then send it out to SCI port
;  Need memory location for counter CTR and buffer BUF(6 byte max)
;**********************************************
pnum10          pshd                   ;Save registers
                pshx
                pshy
                clr     CTR            ; clear character count of an 8 bit number

                ldy     #BUF
pnum10p1        ldx     #10
                idiv
                beq     pnum10p2
                stab    1,y+
                inc     CTR
                tfr     x,d
                bra     pnum10p1

pnum10p2        stab    1,y+
                inc     CTR                        
;--------------------------------------

pnum10p3        ldaa    #$30                
                adda    1,-y
                jsr     putchar
                dec     CTR
                bne     pnum10p3
                jsr     nextline
                puly
                pulx
                puld
                rts
;***********end of pnum10********************
            
            
binaryTable
                
                tfr     A,X

                ldd     #$0000              ;replaces number with correct binary representation
                cpx     #$0
                beq     fin
                
                ldd     #$0001     
                cpx     #$1
                beq     fin
                
                ldd     #$0010     
                cpx     #$2
                beq     fin
                
                ldd     #$0011     
                cpx     #$3
                beq     fin
                
                ldd     #$0100     
                cpx     #$4
                beq     fin
                
                ldd     #$0101     
                cpx     #$5
                beq     fin
                
                ldd     #$0110     
                cpx     #$6
                beq     fin
                
                ldd     #$0111     
                cpx     #$7
                beq     fin
                
                ldd     #$1000     
                cpx     #$8
                beq     fin
                
                ldd     #$1001     
                cpx     #$9
                beq     fin
                
                ldd     #$1010     
                cpx     #$A
                beq     fin
                
                ldd     #$1011     
                cpx     #$B
                beq     fin
                
                ldd     #$1100     
                cpx     #$C
                beq     fin
                
                ldd     #$1101     
                cpx     #$D
                beq     fin
                
                ldd     #$1110     
                cpx     #$E
                beq     fin
                
                ldd     #$1111     
                cpx     #$F
                beq     fin
                
fin
                rts            
            
;***********errorMsg***************************
;* Program: Will display error message
;* Input: None   
;* Output: displayed error message  
;* Registers modified: X
;**********************************************
errorMsg
            jsr   nextline
            ldy   #msgInv            ; Load in Invalid message
            jsr   printmsg
            jsr   nextline
            lbra  restartLoop

errorMsg2
            jsr   nextline
            ldy   #msgInv2            ; Load in Invalid message
            jsr   printmsg
            jsr   nextline
            lbra  restartLoop
            
errorMsg3
            jsr   nextline
            ldy   #msgInv3            ; Load in Invalid message
            jsr   printmsg
            jsr   nextline
            lbra  restartLoop

;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshy
printmsgloop    ldaa    1,Y+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                bsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    puly 
                pula
                rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                      ; send a character
            rts
;***************end of putchar*****************

;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    psha
            ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            pula
            rts     
            
            
            
;***************data section 2*****************
msgStart       DC.B    'Welcome to serial port based Monitor program',$00
msgInv         DC.B    'ERROR',$00
msgInv2        DC.B    'ERROR, LD formatting wrong',$00
msgInv3        DC.B    'ERROR, too many digits in adress/data, must be 4',$00
tabmsg         DC.B    '   ',$00
msgQuit        DC.B    'Memory manager stopped and Typewriter started.',$00
msgQuit2       DC.B    'Please type anything below!', $00
memlimMsg      DC.B    'Specified memory limit reached, no longer accepting more data',$00
Buff           DS.B    13
       
            
            