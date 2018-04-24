                       .MODEL MEDIUM;MEDIUM :data = 64KB but no code restriction
;------------------------------------------------------
                    .STACK 64   ;64 BYTES for stack      
;------------------------------------------------------                    
                    .DATA
widhig               EQU 10           ;any wall width and highet
white                EQU 0fh          ;the wigte color
nbiuld               EQU 8            ;number of walls on the level is 8*8


;colorval             db 1      
colormode            db 0           ;check the color mode for drawing
colorsbuild          db 2BH,2BH+1,2BH+2,2BH+3,white,white,2BH+4,2BH+5,2BH+6,2BH+7,0 ;the wall colors
;colorsother          db 3CH, 3CH+1, 3CH+2, 3CH+3, 3CH+4, 3CH+5, 3CH+6, 3CH+7, 3CH+8, 3CH+9,0
colorsother          db 4h,4h,2h,2h,6h,6h,4h,4h,2h,2h,6h,6h,4h,4h ;the destroyed wall colors
color                db 10 dup(?),0 ;any other colors want to draw "chosen while run"

places               dw widhig,widhig,2*widhig*nbiuld+widhig,2*widhig*nbiuld+widhig ;the place of first charcter
placeolds            dw widhig,widhig,2*widhig*nbiuld+widhig,2*widhig*nbiuld+widhig ;the place of second charcter

bombTime             EQU 3500         ;time for wait the bomb to exp
bombs1               dw 400,400,400,400,400,400 ;;place of bombs of player1 (x,y)* 3 bombs max
bombs2               dw 400,400,400,400,400,400 ;same for player 2
times1               dw bombTime, bombTime, bombTime ;time of bombs of player1 
times2               dw bombTime, bombTime, bombTime  ;same for player2
bombrange            EQU 3     ;the bomb destroing range
healths              db 4, 4   ;the levels of two players

;mes1                 db "1: $"
;mes2                 db "2: $"
;healthstring         db ?,'$',?,'$' 
;saveMode             db ? 
;level                db ?   


;some important strings to use
welcomemsg           db "welcome","$"  
enter                db 10,13 ,'$'
welcome_st           db "enter 1st name ","$"
welcome_nd           db "enter 2nd name ","$"
st_name              db 14,?,15 dup('$'),'$'
nd_name              db 14,?,15 dup('$'),'$'
levelsmsg1           db "level one$"                          
levelsmsg2           db "level two",'$'
win                  db ' wins$'   
pressto              db 'press n to start game or ESC to exit$'
chat                 db 'chat$'
exit                 db 'exit$' 
noone                db 'no one$'
char                 db ?
;--------------------------------------------------------
                    .CODE                                                 
MAIN                PROC FAR        ;Procdure length (default:NEAR)
                    MOV AX,@DATA    ;WE Need to mov DS,@DATA
                    MOV DS,AX       ;But we can not do it in a single step
                    MOV ES,AX       ;to use string operations esaily 
                    
                    newgame:        ;to start a new game
                    call welcome
                    call printNames 
                    call level1
                    call Characters
                    CALL Game
                    
                    getnext:    ;after game is ended get from user input
                    mov ah, 0
                    int 16h
;                    
                    cmp al, 27  ;if esc exit the game 
                    je finishgame
                    
                    cmp ah, 49  ;if n start new game
                    je newgame  
                    
                    jmp getnext
                    
                    finishgame:
                    mov ah,4ch ;go back 
                    int 21h ; to DOS.
                    ;hlt
MAIN                ENDP
;-------------------------------------------------
winstate proc     ;this function prints the name of the winner of the game from si and the options after the game is over
    mov bh,0
    mov ah,2          ;Move Cursor
    mov dl,24
    mov dh,9
    int 10h           ;BIOS INT

    mov ah,9  
    mov dx,si;Display string 
    int 21h           ;DOS INT


    mov bh,0
    mov ah,2          ;Move Cursor
    mov dl,24
    mov dh,10
    int 10h           ;BIOS INT    
                                            
    mov ah,9
    mov dx, offset win;Display string 
    int 21h           ;DOS INT 


    mov bh,0
    mov ah,2          ;Move Cursor
    mov dl,0
    mov dh,24     
    mov bl,05Fh
    int 10h           ;BIOS INT    
                                            
    mov ah,9
    mov dx, offset pressto;Display string 
    int 21h           ;DOS INT                                        
        
    ret 
    winstate endp  
    ;-------------------------------------------------
    printNames proc   ;this function prints the names of players taken from users
    mov bh,0
    mov ah,2          ;Move Cursor
    mov dl,st_name[1]
    mov dh,3
    int 10h           ;BIOS INT

    mov ah,9
    mov dx,offset st_name[2];Display string 
    int 21h           ;DOS INT

    mov bh,0
    mov ah,2          ;Move Cursor
    mov dl,nd_name[1]
    mov dh,4
    int 10h           ;BIOS INT

    mov ah, 9
    mov dx, offset nd_name[2];Display string 
    int 21h           ;DOS INT
            
    ret
printNames endp
;-------------------------------------------------
welcome  proc   ;this function reset all the values to get the defult values, take the users names, and chose the level


    mov ah,0          ;Change video mode (Graphical MODE)
    mov al,13h         ;Max memory size 16KByte
                                        ;AL:4 (320*200=64000 [2 bits for each pixel,4 colours])
                                        ;AL:6 (640*200=128000[1 bit  for each pixel,2 colours B/W])
    int 10h    
            
    call reset         

    push ax

    mov ah,9
    mov dx,offset welcomemsg
    int 21h

    mov ah,9
    mov dx,offset enter
    int 21h

    mov ah ,9
    mov dx ,offset welcome_st
    int 21h

    first_char: mov ah,07
                int 21h
                checks: cmp al,65  ;;check uppercase
                        jae check
                        jmp first_char
                        check: cmp al,90
                               jbe rest
                               jmp check2 
                               
                  check2: cmp al,97  ;;checklowercase
                        jae check22
                        jmp first_char
                        check22: cmp al,122
                               jbe rest
                               jmp first_char
   
     
   rest:
   
    mov ah,2
    mov dl,al
    int 21h 
    mov char,dl
    mov ah,0ah
    mov dx,offset st_name
    int 21h
            
   mov bl,st_name+1
   mov bh,0
   mov st_name+2+bx,'$'
   
   mov bh,37
   sub bh,bl
   mov [st_name+1],bh
   
   mov cx,15
  
   
   
   mov di,offset st_name+15                                  
   shift_st: mov bl,[di]
             mov [di+1],bl 
             dec di
             loop shift_st  
             
             
   mov al,char         
   mov st_name[2],al         
                               
             
    mov ah,9
    mov dx,offset enter
    int 21h


    MOV st_name[17],'$'

    mov ah ,9
    mov dx ,offset welcome_nd
    int 21h 
    
    first_char2: mov ah,07
                int 21h
                checks_nd: cmp al,65  ;;check uppercase
                        jae check_nd
                        jmp first_char2
                        check_nd: cmp al,90
                               jbe rest2
                               jmp check2_nd 
                               
                  check2_nd: cmp al,97  ;;checklowercase
                        jae check222
                        jmp first_char2
                        check222: cmp al,122
                               jbe rest2
                               jmp first_char2
                                     
                        
    rest2:
   
    mov ah,2
    mov dl,al
    int 21h 
     mov char,al
    mov ah,0ah
    mov dx,offset nd_name
    int 21h           
    
   mov bl,nd_name+1
   mov bh,0
   mov nd_name+2+bx,'$' 
            
   mov bh,37
   sub bh,bl
   mov [nd_name+1],bh
  
   mov cx,15
   mov di,offset nd_name+15                                  
   shift_nd: mov bl,[di]
             mov [di+1],bl 
             dec di
             loop shift_nd  
             
             
   mov al,char          
   mov nd_name[2],al        
                         

   MOV nd_name[17],'$'
    pop ax




    mov ah,2          ;Move Cursor
    mov dl,12
    mov dh,9
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah ,9
    mov dx ,offset levelsmsg1
    int 21h



    mov ah,2          ;Move Cursor
    mov dl,12
    mov dh,10
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah ,9
    mov dx ,offset levelsmsg2
    int 21h
     
     
    mov ah,2          ;Move Cursor
    mov dl,12
    mov dh,11
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah ,9
    mov dx ,offset chat
    int 21h
      
    mov ah,2          ;Move Cursor
    mov dl,12
    mov dh,12
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah ,9
    mov dx ,offset exit
    int 21h
    mov ah,2          ;Move Cursor
    mov dl,12
    mov dh,12
    mov bh,0h
    int 10h     
    mov bl,0
   

    choose:
    push dx
    mov ah,2          ;Move Cursor
    mov dh,9
    mov dl,11
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah,2
    mov dl,' ' 
    int 21h

    mov ah,2          ;Move Cursor
    mov dl,11 
    mov dh,10
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah,2
    mov dl,' '
    int 21h  
    
    mov ah,2          ;Move Cursor
    mov dl,11 
    mov dh,11
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah,2
    mov dl,' '
    int 21h
            
            
     mov ah,2          ;Move Cursor
    mov dl,11 
    mov dh,12
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah,2
    mov dl,' '
    int 21h        
            
    pop dx
    mov cl,dh


    mov ah,2          ;Move Cursor
    mov dl,11
    mov bh,0h
    int 10h           ;BIOS INT

    mov ah,2
    mov dl,16 
    int 21h

    mov ah,0        ;Get key pressed (Wait for a key-AH:scancode,AL:ASCII)
    int 16h

    cmp ah, 80
    je moveDown_c
   

    cmp ah, 72
    je moveUp_c

    cmp al,13

    je fin 
    jmp choose

     movedown_c:cmp cl,9
                je movedown0
                cmp cl,10
                je movedown1
                cmp cl,11
                je movedown22
                cmp cl,12
                je movedown3 
                
    
    moveDown0: 
    mov dh,10
    jmp choose 
    moveDown1: 
    mov dh,11
    jmp choose 
    
     moveDown22: 
    mov dh,12
    jmp choose
    
     moveDown3: 
    mov dh,9
    jmp choose 
    
    
     moveup_c:cmp cl,12
                je moveup1
                cmp cl,11
                je moveup22
                cmp cl,10
                je moveup3
                cmp cl,9
                je movup0
    
    moveUp1:
    mov dh,11
    jmp choose
    moveUp22:
    mov dh,10
    jmp choose
    moveUp3:
    mov dh,9
    jmp choose
    movup0:
    mov dh,12
    jmp choose
    fin:
    call clear
    
    cmp dh,12  ;;check for exit
    je exitgame
    jmp continuegame  
    exitgame:mov ah,4ch
            int 21h 
            

   continuegame: cmp dh,10
    jnz lev1
    call level2
    jmp finishwel

    lev1: call level1

    finishwel:
    call Characters
    mov bh,0
    mov ah,2          ;Move Cursor
    mov dl,0
    mov dh,24     
    mov bl,05Fh
    int 10h           ;BIOS INT    
           
    mov ah,9
    mov dx, offset pressto;Display string 
    int 21h  
    ret
welcome        ENDP
;---------------------------------------------------------------------------
clear      proc    ;clear all the screen "draw a black rectangle"
    push cx
    push dx
    
    mov cx,0
    mov dx,0
    mov bl,255
    mov bh,200
    mov al ,0
    mov colormode,1
    call Dsqure2
    
    mov cx,255
    mov bl,65
    mov bh,200
    mov al,0
    mov colormode,1
    call Dsqure2
    pop dx
    pop cx
    ret
clear ENDP
;---------------------------------------------------------------------------
readbyte PROC ;call readbyte int. but mov the cx and dx inside the squre to check the middle point of any squre 
 mov ah,0dh ;input cx,dx, output color in al
 add cx,widhig/2
 add dx,widhig/2
 int 10h        
 sub cx,widhig/2
 sub dx,widhig/2
 ret
readbyte ENDP
;-------------------------------------------------
Dsqure2               PROC ;draw a squre
                        ;color mode on value [colormode], cloumn on cx,row on dx,size of squre is bl horizontal,bh vertical 
                        
                        push bp
                        push ax 
                        push si
                        
                        ;if color mode is 1 the colors inside memory of [di], if color mode is 0 take the color from al and put it into di
                        mov si,cx 
                        mov cx,10
                        cmp colormode,0
                        jnz loadcolorbuffer
                        jmp continue
                        
                       loadcolorbuffer: mov di,offset color
                                         rep stosb
                                         mov di,offset color 
                        
                        continue:mov cx,si ;back to any value
                                 mov ah,0ch ;set draw pixel options
                        
                        pop si
                        
                        
                        push di 
                        push dx
                        push bx
                        push cx
                        
                        mov bp,sp ;to access the stack without pop and push
                                 
                            MainLoop2: ;draw the virtcal lines
                                subloop2:      ;draw horizontal ones
                                int 10h
                                inc cx
                                dec bl
                                jnz subloop2
                            inc di
                            mov al,[di] ;take the color from the memory
                            mov bl,[bp+2] ;get the bl from the stack again 
                            mov cx,[bp] ;get cx from stack
                            inc dx
                            
                            cmp [di],0  ;if the color inside memory is finised back to first color again
                            jnz ssss:
                            mov di,[bp+6]   ;back to first color from the stack
                            mov al,[di]
                            ssss:
                            dec bh
                            jnz  MainLoop2 

                        mov colormode,0 ;reset the color mode
                        pop cx
                        pop bx
                        pop dx
                        pop di                        
                        pop ax
                        pop bp
                      RET
Dsqure2               ENDP                    
;-------------------------------------------------
Frame               PROC ;draw the frame of the game 
                    lea di,colorsbuild  ;take the wall color from memory
                   
                    ;draw the uper horizontal frame 
                    mov cx,0
                    mov dx,0
                    mov al,white
                    mov bl,2*widhig*nbiuld+2*widhig
                    mov bh,widhig
                    Call dsqure2 
                    
                    ;draw the left vertical frame 
                    mov cx,0
                    mov dx,0
                    mov al,white
                    mov bl,widhig
                    mov bh,2*widhig*nbiuld+2*widhig
                    Call dsqure2 
                    
                    ;draw the right vertical frame 
                    mov cx,0
                    mov dx,2*widhig*nbiuld+2*widhig
                    mov al,white
                    mov bl,2*widhig*nbiuld+2*widhig
                    mov bh,widhig
                    Call dsqure2 
                    
                    ;draw the lower horizontal frame                     
                    mov cx,2*widhig*nbiuld+2*widhig
                    mov dx,0
                    mov al,white
                    mov bl,widhig
                    mov bh,2*widhig*nbiuld+3*widhig
                    Call dsqure2 
                    
                    RET
Frame               ENDP                    
;-------------------------------------------------
Level1              PROC       ;draw the level1 "all the undestroyed walls"
                    CALL frame
                    lea di,colorsbuild
                    mov cx,2*widhig ;start from the first of the grad
                    mov dx,2*widhig
                    

                    
                    mov bl,nbiuld   ;select the size of the loops "grad size"
                    mov bh,nbiuld
                    push cx
                    mov bp,sp       ;again to access the stack without pop and push
                    aloop:          ;virtiacl lines inside grad
                        asubloop:   ;horzontal one
                            push bx ;save values of loop
                            mov bl,widhig   ;configration to draw wall
                            mov bh,widhig
                            CALL Dsqure2
                            pop bx
                            add cx,2*widhig
                            dec bl
                            jnz asubloop
                        mov cx,2*widhig ;back to strart of grad
                        add dx,2*widhig
                        mov bl,nbiuld
                        dec bh
                        jnz aloop
                    
                    ;make two gaps to go inside it
                    mov cx, 13*widhig
                    mov dx, 0
                    call clearsqure
                    mov cx, 5*widhig
                    mov dx, 2*widhig*nbiuld+2*widhig        
                    call clearsqure

                    pop cx
                    RET
Level1              ENDP                    
;-------------------------------------------------
Level2              PROC       ;draw the destroyed walls only for level2
                    ;the function draw a random squres to destroy
                    ;the squres generated randomly first time "before code" and then we hardcoded drawing of it
                    
                    lea di,colorsother
                    
                    ;each group configrate the squres in a side of the screen and the call the drawing procedure
                    mov cx,2*widhig
                    mov dx,2*widhig
                    mov bl,2*widhig
                    mov bh,2*widhig
                    CALL Dsqure2
                    
                    mov cx,2*widhig
                    mov dx,5*widhig
                    mov bl,2*widhig
                    mov bh,3*widhig
                    CALL Dsqure2                           
                    
                    mov cx,7*widhig
                    mov dx,1*widhig
                    mov bl,3*widhig
                    mov bh,7*widhig
                    CALL Dsqure2
                    
                    mov cx,13*widhig
                    mov dx,1*widhig
                    mov bl,2*widhig
                    mov bh,7*widhig
                    CALL Dsqure2                   
                    

                    mov cx,2*widhig
                    mov dx,9*widhig
                    mov ah,4
                    a2loop:
                        mov bl,widhig
                        mov bh,widhig
                        CALL Dsqure2
                        
                        add cx,2*widhig
                        CALL Dsqure2
                        sub cx,2*widhig
                        
                        add dx,2*widhig
                        mov bl,3*widhig
                        mov bh,6*widhig
                        CALL Dsqure2
                        sub dx,2*widhig
                        
                        add cx,4*widhig
                        
                        dec ah
                        jnz a2loop
                                   
                    mov cx,5*widhig
                    mov dx,2*widhig        
                    mov ah,4
                    a2loop2:
                        mov bl,widhig
                        mov bh,widhig
                        CALL Dsqure2
                        
                        add cx,6*widhig
                        CALL Dsqure2
                                   
                        add cx,5*widhig
                        CALL Dsqure2
                        sub cx,11*widhig
                        
                        add dx,2*widhig
                        dec ah
                        jnz a2loop2
                    call level1    
                    RET
Level2              ENDP                    
;-------------------------------------------------
Characters           PROC   ;prepare to draw the two characters
                      
                      ;clear the old place of the first character
                      mov cx,[placeolds]
                      mov dx,[placeolds+2] 
                      call clearsqure
                      
                      ;clear the old place of the second character
                      mov cx,[placeolds+4]
                      mov dx,[placeolds+6] 
                      call clearsqure
                                                    
                      ;set options for character1 
                      cmp healths[0], 0 ;not to draw it if he died 
                      je tochar2
                      mov cx,[places]
                      mov dx,[places+2] 
                      mov al,09h        ;chose it's color
                      call onecharacter 
                      
                      ;same for character2
                      tochar2:
                      cmp healths[1], 0
                      je gameover
                      mov cx,[places+4]
                      mov dx,[places+6] 
                      mov al,0Ch
                      call onecharacter
                      
                    gameover:       
                    RET
characters           ENDP                    
;-------------------------------------------------
bomb                PROC        ;this function take cx,dx the bomb place and draw the bomb
                    push cx
                    push dx

                    ;yellow Explosion
                    add cx,1
                    add dx,2
                    add cx,5
                    mov bl,3
                    mov bh,1
                    mov al,00001110b
                    mov colormode,1
                    CALL Dsqure2
                    
                    ;draw the body of the bomb with scan line algorithm
                    mov ah,5
                    bloop:
                        ;color of start of bomb
                        sub cx,1
                        inc dx
                        mov bl,1
                        mov bh,1
                        mov al,00001001b
                        mov colormode,1
                        CALL Dsqure2
                    
                        ;inside the bomb
                        inc cx
                        mov bl,3
                        mov bh,1
                        mov al,00001100b
                         mov colormode,1
                         CALL Dsqure2
                    
                        ;end of bomb
                        add cx,3
                        mov bl,1
                        mov bh,1
                        mov al,00001001b
                        mov colormode,1
                        CALL Dsqure2
                    
                        
                        sub cx,4
                        dec ah
                        jnz bloop
                        
                    inc dx
                    mov bl,4
                    mov bh,1
                    mov al,9
                     mov colormode,1
                    CALL Dsqure2
                    
                    pop dx
                    pop cx
                           
                    RET
bomb                ENDP                    
;------------------------------------------------- 
clearsqure           PROC        ;take cx,dx as inputs and clear this squre
                    mov al,0 ;set black color
                    mov colormode,1 ;configrate the squre drawing mode and size of squre
                    mov bh,widhig
                    mov bl,widhig
                    call dsqure2
                    RET
clearsqure           ENDP
;-------------------------------------------------
bombex              PROC       ;this function is to draw and to clear the bomb explosion if al=0 is to clear else to draw
                    mov colormode,1 ;set color mode
                    push cx
                    push dx
                    mov bp,sp       ;to access stack without push and pop alot
                    mov bl,widhig   ;set configration of drawn squres
                    mov bh,widhig

                    ;the right side of the bomp
                    mov ah,bombrange
                    beloop1:
                        push ax     ;save looping and color values
                        add cx,widhig
                        call readbyte   ;check is the squre is drawn or not and get is's color on al
                        cmp al,0fh      ;if the color is white don't draw and exit
                        je beexit1      
                        pop ax
                        CALL Dsqure2
                        dec ah
                        jnz beloop1
                        
                        push ax 
                        
                    
                    beexit1:        ;get the first configrations from stack
                        pop ax
                        mov dx,[bp]
                        mov cx,[bp+2]
                    
                    
                    ;the left side of the bomp
                    mov ah,bombrange                  
                    beloop2:
                        push ax
                        
                       add dx,widhig
                        call readbyte
                        
                        cmp al,0fh
                        je beexit2 
                        cmp dx, 2*widhig*nbiuld+3*widhig
                        je beexit2
                        pop ax
                        
                        CALL Dsqure2
                        dec ah
                        jnz beloop2
                        push ax
                        
                    beexit2:
                        pop ax
                        mov dx,[bp]
                        mov cx,[bp+2]
                        
                    ;the top side of the bomp    
                    mov ah,bombrange         
                    beloop3:
                        push ax
                        sub dx,widhig
                        call readbyte
                        
                        cmp al,0fh
                        je beexit3 
                        cmp dx, 0-widhig
                        je beexit3
                        pop ax
                       
                        CALL Dsqure2
                        dec ah
                        jnz beloop3
                        push ax 
                         
                    
                    beexit3:
                        pop ax
                        mov dx,[bp]
                        mov cx,[bp+2]
                        
                    ;the bottom side of the bomp    
                    mov ah,bombrange
                    beloop4:
                        push ax 
                        sub cx,widhig
                        call readbyte 
                        
                        cmp al,0fh
                        je beexit4
                        pop ax
                       
                        CALL Dsqure2
                        dec ah
                        jnz beloop4
                        push ax
                    
                    beexit4:
                        pop ax
                        mov dx,[bp]
                        mov cx,[bp+2]
                        
                    
                    pop dx
                    pop cx
                    
                    RET
bombex              ENDP                         
;-------------------------------------------------
oneCharacter        PROC        ;the function draw the charater it's input color on al, the place on cx,dx
                    push ax
                    mov bp,sp   ;to use stack without pop and push 
                    mov colormode,1 ;set color mode
                    ;we draw the character with pixels and coloerd him with pixels inside a drawing program
                    ;then we take this pixels and draw it hardcoded with assembly 
                    ;each group of code is to set place with cx,dx and size with bh,bl and color with al and then call draw square
                    ;using scan line algorithm each group of code is a scan line (or two lines) on the character
                    add cx,4
                    mov bl,4
                    mov bh,1
                    CALL Dsqure2
                    
                    sub cx,1
                    inc dx
                    mov bl,6
                    mov bh,1
                    CALL Dsqure2
                    
                    mov al,0Ah
                    inc dx
                    add cx,1
                    mov bl,1
                    mov bh,3
                    CALL Dsqure2
                    
                    mov al,04h
                    add cx,1
                    mov bl,3
                    mov bh,2
                    CALL Dsqure2
                    
                    mov al,[bp]     ;to get the original color of character "change from player1 to player2"
                    add dx,2
                    mov bl,3
                    mov bh,2
                    CALL Dsqure2
                                          
                    mov al,04h
                    add dx,2
                    mov bl,3
                    mov bh,2
                    CALL Dsqure2
                    
                    mov al,[bp]     ;to get the original color of character "change from player1 to player2"
                    add dx,2
                    mov bl,3
                    mov bh,2
                    CALL Dsqure2
                    
                    pop ax   
                    RET
oneCharacter        ENDP                    
;-------------------------------------------------    
;------------------------------------------------
;needs a point in memory named point x,y  and calls function draw
movePlayer1 proc 
    
        ;moves the place of player one from places to oldplaces
        mov dx, places[0]              
        mov placeolds[0], dx  
        mov dx, places[2]           ;here dx holds the y coordinate of player one
        mov placeolds[2], dx
        
        ;detect which key is pressed to move player one
        cmp ah, 77 
        je moveRight
        cmp ah, 80
        je moveDown
        cmp ah, 72
        je moveUp
        cmp ah, 75
        je moveLeft  
        jmp endProc
        

        moveUp:            ;if up was pressed
        cmp dx, 0          ;if player one is in the tunnel
        je upnormal
        sub dx, 10            ;sub 10 to try to move up
        mov cx, places[0]     ;move x coordinate to cx 
        call readbyte         ;read color in the middle of the above square
        cmp al, 00h
        jne endproc           ;don't move if it's not black
        mov places[2], dx     ;you can move up then move dx to y coordinate
        jmp endProc           ;exit moving
        upnormal:             ;in the tunnel
        mov cx, 5*widhig                         ;move x of the other tunnel
        mov dx, 2*widhig*nbiuld+2*widhig         ;move y of the other tunnel
        call readbyte                            ;read color in the down tunnel
        cmp al, 00h                   
        jne endproc                  ;don't move if there is something there
        mov places[0], cx            ;move x to player
        mov places[2], dx            ;move y to player
        jmp endProc
   
   
        moveDown:     
        cmp dx, 2*widhig*nbiuld+2*widhig          ;check if player is in the town tunnel
        je upnormal2                              
        add dx, 10                                ;get y of the down square in dx
        mov cx, places[0]                         ;get x of the down square in cx
        call readbyte                             ;read color of the square
        cmp al, 00h                               ;check with black
        jne endproc                               ;don't move if there is something
        mov places[2], dx                         ;move y to the player
        jmp endproc                               ;end moving
        upnormal2:                                ;if it is in the down tunnel
        mov cx, 13*widhig                         ;mov x of the up tunnel in cx
        mov dx, 0                                 ;mov y of the up tunnel in dx
        call readbyte                             ;read color
        cmp al, 00h                               ;compare with black
        jne endproc                               ;don't move if there is something
        mov places[0], cx                         ;move x to the player
        mov places[2], dx                         ;move y to the player
        jmp endProc                               ;end moving
        
        moveRight: 
        mov cx, places[0]                         ;move x of player to cx
        add cx, 10                                ;get x of the right square
        call readbyte                             ;read its color
        cmp al, 00h                               ;compare with black
        jne endproc                               ;don't move if there is something
        mov places[0], cx                         ;move x to player to move right
        jmp endProc                               ;end moving
        
        moveLeft:
        mov cx, places[0]                         ;move x of player to cx
        sub cx, 10                                ;get x of the left square
        call readbyte                             ;read its color
        cmp al, 00h                               ;compare with black
        jne endproc                               ;don't move if there is something
        mov places[0], cx                         ;move x to player to move left
         
        endProc:

    
    ret    
endp movePlayer1  
;-------------------------------------------------
;needs a point in memory named point x,y  and calls function draw
movePlayer2 proc 
        ;move the current place of player two to placeolds
        mov dx, places[4]
        mov placeolds[4], dx  
        mov dx, places[6]
        mov placeolds[6], dx        ;dx has the y of player two
        
        ;check which key is pressed to move
        cmp al, 100 
        je moveRight2
        cmp al, 115
        je moveDown2
        cmp al, 119
        je moveUp2
        cmp al, 97
        je moveLeft2 
        jmp endProc2
        
        moveUp2: 
        cmp dx, 0                              ;check if player two is in the up tunnel
        je upnormal3                           ;go to move to tunnel
        sub dx, 10                             ;get y of the up square
        mov cx, places[4]                      ;get x of the up square
        call readbyte                          ;read its color
        cmp al, 00h                            ;compare with black
        jne endproc                            ;don't move if there is something
        mov places[6], dx                      ;move y to player
        jmp endproc2                           ;end moving
        upnormal3:   
        mov cx, 5*widhig                       ;get x of the down tunnel
        mov dx, 2*widhig*nbiuld+2*widhig       ;get y of the down tunnel
        call readbyte                          ;reat its color
        cmp al, 00h                            ;compare with black
        jne endproc2                            ;don't move if there is something
        mov places[4], cx                      ;move x to player
        mov places[6], dx                      ;move y to player
        jmp endProc2                           ;end moving
   
        moveDown2: 
        cmp dx, 2*widhig*nbiuld+2*widhig       ;check if player is in the down tunnel
        je upnormal4                           ;moving in tunnel
        add dx, 10                             ;get y of the down square
        mov cx, places[4]                      ;get x of the up square
        call readbyte                          ;read its color
        cmp al, 00h                            ;compare with black
        jne endproc2                           ;don't move if there is something
        mov places[6], dx                      ;move y to player
        jmp endproc2                           ;end moving
        upnormal4: 
        mov cx, 13*widhig                      ;get x of the up tunnel
        mov dx, 0                              ;get y of the up tunnel
        call readbyte                          ;read its color
        cmp al, 00h                            ;compare with black
        jne endproc2                           ;don't move if there is something
        mov places[4], cx                      ;move x to player
        mov places[6], dx                      ;move y to player
        jmp endProc2                           ;end moving
        
        moveRight2: 
        mov cx, places[4]                      ;move x of player to cx
        add cx, 10                             ;get x of the right square
        call readbyte                          ;read its color
        cmp al, 00h                            ;compare with black
        jne endproc                            ;don't move if there is something
        mov places[4], cx                      ;move x to player
        jmp endProc2                           ;end moving
        
        moveLeft2:   
        mov cx, places[4]                      ;move x of player to cx
        sub cx, 10                             ;get x of the left square
        call readbyte                          ;read its color
        cmp al, 00h                            ;compare with black
        jne endproc                            ;don't move if there is something
        mov places[4], cx                      ;move x to player
         
        endProc2:

    
    ret    
endp movePlayer2
;------------------------------------------------------------------------------------------- 
throwBomb proc
    cmp al, 47       ;check if player one throws a bomb
    je throw1
    cmp al, 32       ;check if player two throws a bomb
    je throw2
    jmp endProc3

    throw1:
    mov bx, offset bombs1           ;array of bombs of player one
    mov cl, 0                       ;use in loop counting
    bombs1loop:
    cmp [bx], 400                   ;check if that place is empty
    jne notempty                    ;don't throw if not empty
    mov dx, places[0]               ;get x of player one
    mov [bx], dx                    ;move x to bomb place
    mov dx, places[2]               ;get y of player one
    mov [bx+2], dx                  ;move y to bomb place
    jmp endProc3                    ;end throw
    notempty:
    add bx, 4                       ;move to the next place in array of bombs
    inc cl
    cmp cl, 3                       ;max number is 3 bombs
    jne bombs1loop                  ;repeat if less than 3
    jmp endProc3                    ;end throwing


    throw2:
    mov bx, offset bombs2          ;array of bombs of player two
    mov cl, 0                      ;use in loop counting
    bombs2loop:
    cmp [bx], 400                  ;check if that place is empty
    jne notempty2                  ;don't throw if not empty
    mov dx, places[4]              ;get x of player two
    mov [bx], dx                   ;move x to bomb place;move x to bomb place
    mov dx, places[6]              ;get y of player two
    mov [bx+2], dx                 ;move y to bomb place
    jmp endProc3                   ;end throw
    notempty2:
    add bx, 4                      ;move to the next place in array of bombs
    inc cl
    cmp cl, 3                      ;max number is 3 bombs
    jne bombs2loop                 ;repeat if less than 3
    jmp endProc3                   ;end throwing
        
    
    
    endProc3:   
    ret
endp throwBomb           
;-------------------------------------------------  
BombDiffuse     proc               ;this funcition is called in every main loop to check when to explode bombs
    mov cl, 0                          ;use in loop count
    mov bx, offset bombs1              ;get array of bombs
    mov si, offset times1              ;get array of times
    fight:
    cmp [bx], 400                      ;check if there is bomb
    je resume                          ;do nothing if no bomb
    pusha
    mov cx, [bx]                       ;get x of the bomb
    mov dx, [bx+2]                     ;get y of the bomb
    call bomb                          ;draw bomb
    popa
    mov dx, [si]                       ;get time of bomb
    dec dx                             ;decrease it
    cmp dx, 0                          ;compare if time is end
    je removebomb                      ;remove bomb and explosion
    cmp dx, 500                        ;compare with time of explosion
    je realbomb                        ;explode if equal
    afterbomb:
    mov [si], dx                       ;move time to bomb time
    resume:
    add bx, 4                          ;move to the next bomb
    add si, 2                          ;move to the next bomb time
    inc cl
    cmp cl, 6                          ;max 6 bombs
    jne fight                          ;repeat if not equal
    jmp endProc4                       ;end

    realbomb:
    pusha
    mov cx, [bx]                       ;move x of bomb to cx
    mov dx, [bx+2]                     ;move y of bomb to dx
    mov al, 28h                        ;parameter to bombex
    call bombex                        ;explode bomb
    popa
    call affectPlayer                  ;to decrease player health
    jmp afterbomb                      ;end explosion
    removebomb: 
    pusha 
    mov cx, [bx]                       ;move x of bomb to cx
    mov dx, [bx+2]                     ;move y of bomb to dx
    mov al, 00h                        ;parameter to bombex
    call bombex                        ;to clear explosion
    call clearsqure                     ;to clear bomb
    call Characters                     ;to draw characters
    popa
    mov [bx], 400                      ;set with default
    mov [bx+2], 400                    ;set with default
    mov [si], bombtime                 ;set with default
    jmp resume   


    endProc4:
    ret
endp BombDiffuse
;-------------------------------------------------
affectPlayer proc   
    pusha

    mov cx, [bx]                   ;get x of bomb in cx
    mov dx, [bx+2]                 ;get y of bomb in dx
    mov si, bombrange              ;move range of bomb effect to si
    inc si                         ;increment to affect in the bomb place also

    rightdir:
    mov ah, 0dh                    ;get color interrupt
    int 10h 
    cmp al, 0fh                    ;compare with white
    je befleft                     ;if wall don't continue
    cmp cx, places[0]              ;compare x of bomb with x of player one
    jne contright                  ;do nothing if not equal
    cmp dx, places[2]              ;compare y of bomb with y of player one
    jne contright                  ;do nothing if not equal
    dec healths[0]                 ;decrease health
    contright:
    cmp cx, places[4]              ;compare x of bomb with x of player two
    jne contright2                 ;do nothing if not equal
    cmp dx, places[6]              ;compare y of bomb with y of player one
    jne contright2                 ;do nothing if not equal
    dec healths[1]                 ;decrease health
    contright2:
    add cx, 10                     ;move to the right square
    dec si                         ;decrease range
    cmp si, 0                      ;if last of range
    jne rightdir                   ;repeat if not zero

    befleft:
    mov cx, [bx]                   ;move x of bomb to cx
    sub cx, 10                     ;get x of left square
    mov dx, [bx+2]                 ;move y of bomb in dx
    mov si, bombrange              ;move bomb range in si

    leftdir:
    mov ah, 0dh                    ;read color
    int 10h 
    cmp al, 0fh                    ;compare with white
    je befup                       ;if wall break
    cmp cx, places[0]              ;compare x of bomb with x of player one
    jne contleft                   ;do nothing if not equal
    cmp dx, places[2]              ;compare y of bomb with y of player one
    jne contleft                   ;do nothing if not equal
    dec healths[0]                 ;decrease health
    contleft:
    cmp cx, places[4]              ;compare x of bomb with x of player two
    jne contleft2                  ;do nothing if not equal
    cmp dx, places[6]              ;compare y of bomb with y of player one
    jne contleft2                  ;do nothing if not equal
    dec healths[1]                 ;decrease health
    contleft2:
    sub cx, 10                     ;move to the left square
    dec si                         ;decrease range
    cmp si, 0                      ;if last of range
    jne leftdir                    ;repeat if not zero


    befup:
    mov cx, [bx]                   ;move x of bomb to cx
    mov dx, [bx+2]                 ;move y of bomb in dx
    sub dx, 10                     ;get y of the up square
    mov si, bombrange              ;move bomb range in si

    updir:
    mov ah, 0dh                    ;read color
    int 10h 
    cmp al, 0fh                    ;compare with white
    je befdown                     ;if wall break
    cmp cx, places[0]              ;compare x of bomb with x of player one
    jne contup                     ;do nothing if not equal
    cmp dx, places[2]              ;compare y of bomb with y of player one
    jne contup                     ;do nothing if not equal
    dec healths[0]                 ;decrease health
    contup:
    cmp cx, places[4]              ;compare x of bomb with x of player two
    jne contup2                    ;do nothing if not equal
    cmp dx, places[6]              ;compare y of bomb with y of player one
    jne contup2                    ;do nothing if not equal
    dec healths[1]                 ;decrease health
    contup2:
    sub dx, 10                     ;move to the up square
    dec si                         ;decrease range
    cmp si, 0                      ;if last of range
    jne updir                      ;repeat if not zero


    befdown:                       
    mov cx, [bx]                   ;move x of bomb to cx
    mov dx, [bx+2]                 ;move y of bomb in dx
    add dx, 10                     ;get y of the down square
    mov si, bombrange              ;move bomb range in si

    downdir:
    mov ah, 0dh                    ;read color
    int 10h 
    cmp al, 0fh                    ;compare with white
    je endProc5                    ;if wall break
    cmp cx, places[0]              ;compare x of bomb with x of player one
    jne contdown                   ;do nothing if not equal
    cmp dx, places[2]              ;compare y of bomb with y of player one
    jne contdown                   ;do nothing if not equal
    dec healths[0]                 ;decrease health
    contdown:
    cmp cx, places[4]              ;compare x of bomb with x of player two
    jne contdown2                  ;do nothing if not equal
    cmp dx, places[6]              ;compare y of bomb with y of player one
    jne contdown2                  ;do nothing if not equal
    dec healths[1]                 ;decrease health
    contdown2:
    add dx, 10                     ;move to the down square
    dec si                         ;decrease range
    cmp si, 0                      ;if last of range
    jne downdir                    ;repeat if not zero

    endProc5:
    popa    
    ret
endp affectPlayer
;-------------------------------------------------
printhealth           proc
                      pusha 
                      mov ah,2          ;Move Cursor
                      mov dl,38
                      mov dh,3
                      mov bh,0h
                      int 10h           ;BIOS INT
                      
                      mov ah,2          ;Display Char 
                      mov dl,":"
                      int 21h
                      
                      
                      mov ah,2          ;Display Char 
                      mov dl,healths
                      add dl,'0'
                      int 21h
                      
                      mov ah,2          ;Move Cursor
                      mov dl,38
                      mov dh,4 
                      mov bh,0h
                      int 10h           ;BIOS INT
                      
                      mov ah,2          ;Display Char 
                      mov dl,":"
                      int 21h
                      
                      mov ah,2          ;Display Char
                      mov dl,healths[1]
                      add dl,'0'
                      int 21h                      
                      popa                        
                      ret
printhealth           endp    
;-------------------------------------------------
reset proc             ;this function resets all variables with default values to repeat the game
    pusha  
    ;places of two chracters
    mov places[0], widhig
    mov places[2], widhig
    mov places[4], 2*widhig*nbiuld+widhig
    mov places[6], 2*widhig*nbiuld+widhig    
    mov placeolds[0], widhig
    mov placeolds[2], widhig
    mov placeolds[4], 2*widhig*nbiuld+widhig
    mov placeolds[6], 2*widhig*nbiuld+widhig  

    ;default values of bombs
    mov bx, offset bombs1
    mov cx, 12
    reset1:
    mov [bx], 400
    add bx, 2
    loop reset1 
    mov cx, 6

    ;default values of time
    reset2:
    mov [bx], bombtime
    add bx, 2
    loop reset2  

    ;players healths
    mov healths[0], 4
    mov healths[1], 4 

    ;names with dollar signs
    mov di, offset st_name[2]
    mov cx, 15
    mov al, '$'
    rep stosb 
    mov di, offset nd_name[2]
    mov cx, 15
    mov al, '$'
    rep stosb  
    popa    
    ret 
reset endp  
;-------------------------------------------------
Game                PROC        
                    
                    Gloop:  ;inside the game the main loop
                      
                      
                      CHECK1:       ;check the health of the two players 
                      call printhealth
                      cmp healths, 0
                      je endgame1   
                      cmp healths[1], 0
                      je endgame2


                      call bombDiffuse  ;this the function to check the bombs explosion is now or not
                      mov ah,1
                      int 16h       ;if no key preesed jmp to check health again
                      jz CHECK1 
                      
                     ;if key is pressed move the players
                     call movePlayer1 
                     call movePlayer2
                     call throwBomb        
                     
                     mov ah, 0 ;to take the prressed key from the buffer
                     int 16h
                     
                    cmp al, 27  ;if esc exit the game 
                    je finishgame
                    
                    cmp ah, 49  ;if n start new game
                    je newgame  
       
                      
                     call Characters ;draw the characters
                     
                        
                      
                    jmp Gloop ;back to main loop of game
                      
                    endgame1:  ;game ends with player one lose
                    cmp healths[1], 0 ;check if the second player also lose then is's tie
                    je endgame3
                    mov si, offset nd_name[2]   ;set the second player winner
                    jmp endgame

                    endgame2:  ;game ends with player two lose
                    cmp healths, 0 ;check if the first player also lose then is's tie
                    je endgame3
                    mov si, offset st_name[2] ;set the first player winner
                    jmp endgame
                    
                    endgame3: ;game ends tie
                    mov si, offset noone ;set noone winner
                    
                    endgame:
                    call winstate   ;print the winning state
                    RET
Game                ENDP                    
;-------------------------------------------------
                    END MAIN        ; End of the program  