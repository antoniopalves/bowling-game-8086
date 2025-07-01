               ; multi-segment executable file template.

data segment
  ;*********** Menu **************  
    jogar db    " Jogar $",0
    top5 db     " Top5 $", 0
    creditos db " Creditos $", 0
    sair db     " Sair $", 0 
    
    box_x dw  80
    box_y dw ?
    
    box1_y dw 30
    
    box2_y dw 70
    
    box3_y dw 110
    
    box4_y dw 150
    
    box_width  dw 01Eh ;largura
    box_height dw 0A0h ;comprimento 
   
;*************** Janela do Jogo *****************    
    ;Bola
    WINDOW_WIDTH DW 140h              ;the width of the window (320 pixels)
	WINDOW_HEIGHT DW 0C8h             ;the height of the window (200 pixels)
	WINDOW_BOUNDS DW 6                ;variable used to check collisions early
	
    ball_x dw 0Dh ;posicao x (coluna) do primeiro pixel da bola
    ball_y dw 69h ;posicao y (linha) do primeiro pixel da bola
    
    
    ball_main_size dw 10h; size of the ball  
    
    ball_x_velocity dw 10h
    ball_y_velocity dw 01h
    
    ball_original_x_center dw 1Ch
    ball_original_y_center dw 71h 
    ball_x_center dw 1Dh
    ball_y_center dw 71h
      
    pin_x_size dw 05h
    pin_y_size dw 08h 
    
    ;Pins
    pin_x dw ?
    pin_y dw ?
    
    pin1_x dw 12Ch
    pin1_y dw 3dh
  
    
    pin2_x dw 12Ch
    pin2_y dw 5Dh
    
    pin3_x dw 12Ch
    pin3_y dw 7dh
    
    pin4_x dw 12Ch
    pin4_y dw 9Dh
    
    pin5_x dw 11Ch
    pin5_y dw 4Dh
    
    pin6_x dw 11Ch
    pin6_y dw 6Dh
    
    pin7_x dw 11Ch
    pin7_y dw 8Dh      
    
    pin8_x dw 10Ch
    pin8_y dw 5Dh
    
    pin9_x dw 10Ch
    pin9_y dw 7Dh
    
    pin10_x dw 0FCh
    pin10_y dw 6Dh
    
    ;X
    xx_x dw 2Ch
    xx_y dw 6Ah 
    
    xy_x dw 25h
    xy_y dw 71h
    
    x_height dw 0Eh 
    x_width dw 00h
    
    x_velocity dw 01h
    
    ;Linhas
    linha_x dw 0
    linha1_y dw 2Dh
    linha2_y dw 0ADh 
    
    time_aux db 0    
    
    reg1 db " Insira o nome do primeiro jogador $", 0
    reg2 db " Insira o nome do segundo jogador $", 0 
    str_rounds db " Insira o numero de rondas(1-10) $", 0 
    rounds_inv db " Numero de rondas invalido!!! $", 0   
    text_winner db " O Vencedor foi: $", 0 
    text_draw db " EMPATE!$", 0
    
    player1 db 40 dup(?), 0Dh, 0Ah
    player2 db 40 dup(?), 0Dh, 0Ah  
    
    points1 db 0
    points2 db 0
    text_points1 db '0', '$' 
    text_points2 db '0', '$'  
    
    points db 00h 
    text_points db '0', '$'
    
    total_rounds db ? 
    str_ronda db " Ronda $", 0 
    text_total_rounds db '0', '$'
    str_div db "/", 0
    
    atual_round db 1
    text_atual_round db '0', '$' 
    
    exit_x dw 00h
    exit_y dw 0BEh  
    
    exit_width  dw 0Ah ;largura
    exit_height dw 030h ;comprimento 
    
    enter dw 00h
   
    sair2 db "(E)xit$",0 
;******************TOP 5 *********************
    ResultFile db "c:\emu8086\MyBuild\Results.txt", 0
    Top5File db "c:\emu8086\MyBuild\top5.bin", 0
    Result db 40 dup(?), 0dh, 0ah
   
    
    stringErro db "Erro ao ler ficheiro",0  
    str_result db 40 dup(?), 0dh, 0ah 
    str_result2 db 40 dup(?), 0dh, 0ah
    count db 0  
    
    Top5Title db "Top 5$", 0
    Top5Labels db "Pos        Nome        Pontuacao$",0
    Top5Scores dw 10 dup(0), 0
    Top5Players db 60 dup(0), 0
    TopFive db 65 dup (0)
    
    NomeJogadorAtualizar db 12 dup(?), 0dh, 0ah
    PontuacaoAtualizar db ? 
    
    NumTemp db 4 dup(0), 0 
  
;***************** Creditos ******************
    TR db " Trabalho Realizado por: $", 0
    nome1 db " Antonio Alves, N 58339 $", 0
    nome2 db " Antonio Prazeres, N 55379 $", 0
    nome3 db " Catia Carraca, N 59205 $", 0 
    
    docentes db " Docentes: $", 0
    doc1 db " Joao Pimentao $", 0
    doc2 db " Pedro Sousa $", 0
    doc3 db " Tiago Ferreira $", 0
    doc4 db " Sergio Onofre $", 0
    
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
   
    ;MENU
    ;Carregar TOP 5
    call Load_Top5
     
    menu:
        mov al, 13h
        call setVideoMode 
    print_menu:
        call clear_buffer
        call clear_screen 
        call draw_menu
        call select_menu
        cmp cx, 1
        jne not1
        
        ;jogo
        
        call clear_buffer
        call reg_player1
        call reg_player2
        call reg_rounds
        call clear_buffer
        call manage_game
        call clear_screen 
   
        mov ah, points1
        cmp ah, points2
        jg winner1 
        je draw
        
        call draw_winner2
        jmp exit_manage
        winner1:  
        call draw_winner1 
        jmp exit_manage
        
        draw:
        call draw_draw
        
        exit_manage:
        call Reg_Score_Player1 
        call Reg_Score_Player2
        call update_top5
        call reset_game
      
        jmp menu
        
    not1:
        ;Top 5
        cmp cx, 2
        jne not2 
        call write_top5
        call draw_top5
        jmp menu
        
    not2:
        ;Creditos
        cmp cx, 3
        jne exit 
        call clear_screen
        call clear_buffer
        call write_creditos 
        jmp menu
    exit:

    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends  

Load_Top5 proc
    mov count, 0
    
    load_cicle:
      
    lea dx, Top5File
    mov al, 2
    call FOpen
    jc CreateTop5
    mov bx, ax
    lea dx, TopFive
    mov cx, 65 
    call FRead
    mov cx, 13 
    lea si, Top5Players
    lea bx, Top5Scores
   
    load_players:
    cmp cx,1
    je load_Scores
    push di
    mov di, dx
    mov al, [di]
    mov [si], al
    pop di
    dec cx
    inc dx
    inc si
    jmp load_players
   
    load_scores:
    push ax
    mov al, count
    inc al
    mov count, al
    pop ax
    push di
    mov di, dx
    mov al, [di]
    mov [bx], al
    pop di
    mov cx, 13
    inc bx
    inc dx
    cmp count, 5
    je final_load
    jmp load_players 
    
    ret
    
    CreateTop5:
    xor cx, cx
    call FCreate
    jmp load_cicle
     
    final_load:
    ret
Load_Top5 endp 

update_top5 proc
        push ax 
        push bx
		push cx
		push si
        
		;Comparar pontuacao
        lea si, Top5Scores
		xor bx, bx
		
		scores_compare:			
			mov al, points1
			mov cx, bx
			cmp al, [si]
			jg best
			inc si
			inc bx
			cmp bx, 5
			jne scores_compare
			jmp exit_update
		
		best:
			xchg al, [si]
			inc si                  
			inc bx
			cmp bx, 5
			jne best
			
		;Ajuste de nomes
		mov al, 12
		mul cx                      ;AX = AL * CX, sendo que cx tem o numero de jogadores que sao melhores que o jogador que esta a jogar
		lea si, Top5Players
		add ax, si
		dec ax
		add si, 48
		lea di, Top5Players
		add di, 60
		
		update_t5:
			mov cx, [si]
			mov [di], cx
			dec si
			dec di
			cmp si, ax
			jne update_t5
		
		mov si, 11 ; Ultimo caracter do nome
		
		new_player:
			mov cl,  player1[si]
			mov [di], cl
			dec si
			dec di
			cmp di, ax
			jne new_player		
		
		exit_update:
    		pop si
    		pop cx
            pop bx
            pop ax
            ret
update_top5 endp
    
    ;;;;;;;;;;;;Apresenta os dados do ficheiro TopFive;;;;;;;;;;;;;;
    
draw_top5 proc
        push bp
        push ax
        push bx
        push dx
        
        call clear_screen 
        call clear_buffer
        mov al, 13h
        call setVideoMode  
        
        mov bl, 0fh
        
        ;Apresentar esquema
        ;Titulo
    
        MOV AH,02h                       ;set cursor position
    	MOV BH,00h                       ;set page number
    	MOV DH,01h                       ;set row 
    	MOV DL,08h						 ;set column
    	INT 10h							 
    		
    	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
    	LEA DX,Top5Title     ;give DX a pointer 
    	INT 21h
    	   
        ;Tabelas 
        
        MOV AH,02h                       ;set cursor position
    	MOV BH,00h                       ;set page number
    	MOV DH,02h                       ;set row 
    	MOV DL,01h						 ;set column
    	INT 10h							 
    		
    	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
    	LEA DX,Top5Labels     ;give DX a pointer 
    	INT 21h   
    	
        ;Posicoes
        mov cx, 1 
        inc dl
        mov al, 31h ;Caracter 1
        xor bh, bh
        mov ah, 09h 
        
        
        EscreverPos:
            add dh, 2
            call setcursor
            int 10h
            inc al
            cmp al, 36h
            jne EscreverPos
        
        add dl, 4
        sub dh, 8
        lea bp, Top5Players
        mov cx, 12
        ;Nomes
        write_names:
            call printstr
            add dh, 2
            add bp, 12
            cmp dh, 11h
            jng write_names
        
        ;Pontuacoes 
        lea di, Top5Scores
        lea bp, NumTemp                    ;Numero a ser escrito no ecra
        mov cx, 4
        add dl, 19
        sub dh, 10
        
        write_scores:   
            lea si, NumTemp                ;Buffer de destino
            mov ax, [di]                   ;Numero a converter
            add di, 2
            push 2                         ;Numero de algarismos a converter
            call get_ascii
            add sp, 2                     
            call printstr
            add dh, 2
            cmp dh, 11h
            jng write_scores

        mov ah, 1 ;
        int 21h   ;Press any key          
        
        pop dx
        pop bx                                                                                  
        pop ax                                      
        pop bp 
        ret
draw_top5 endp 
    
;*****************************************************************
; draw_menu - funcao responsavel por escrever os dados atualizados ficheiro top5.bin
;***************************************************************** 
write_top5 proc 
        pusha ;push all
         
        mov count,0
        lea bx, TopFive
        lea si,Top5Players
        lea dx,Top5Scores
        
        mov cx,13
        
        save_players:  
        cmp cx,1
        je save_scores 
        push di
        mov di,si
        mov al,[di]
        mov [bx],al      
        pop di
        dec cx
        inc bx
        inc si
        jmp save_players
        
        save_scores: 
        push ax
        mov al, count
        inc al
        mov count, al
        pop ax
        push di
        mov di,dx
        mov al,[di]       
        mov [bx],al      
        pop di
        mov cx,13 
        inc dx
        inc bx 
        cmp count,5
        je update
        jmp save_players
        
        update:
        lea dx, Top5File
        mov al, 2
        call FOpen
        mov bx,ax         ;ax = file handler 
        xor al,al
        call FSeek
        lea dx, TopFive
        mov cx, 65
        call FWrite
        call fClose
        
        popa    
        ret
write_top5 endp
    

;*****************************************************************
; draw_menu - funcao responsavel por desenhar o menu
;***************************************************************** 
draw_menu proc
    ;desenha caixas 
    mov dx, box1_y
    mov box_y, dx
    call draw_box
     
    mov dx, box2_y
    mov box_y, dx
    call draw_box 
    
    mov dx, box3_y
    mov box_y, dx
    call draw_box
    
    mov dx, box4_y
    mov box_y, dx
    call draw_box  
    
    
    ; escrever jogar 
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,01Fh                       ;set row 
	MOV DL,059h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,jogar      ;give DX a pointer 
	INT 21h                          ;print the string
    
    ;escrever top 5 
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,024h                       ;set row 
	MOV DL,059h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,top5      ;give DX a pointer 
	INT 21h                          ;print the string
    
    
    ;escrever credito
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,029h                       ;set row 
	MOV DL,058h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,creditos     ;give DX a pointer 
	INT 21h                          ;print the string
    
    ;escrever sair
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,02Eh                       ;set row 
	MOV DL,059h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,sair      ;give DX a pointer 
	INT 21h                          ;print the string
    
    ret
draw_menu endp  

;*****************************************************************
; draw_box - funcao responsavel desenhar as caixas do menu
;***************************************************************** 
draw_box proc 
		
    draw_boxs:
    call config_white 
			
	inc cx             ; cx = cx +1 
	mov ax, cx	       ; cx - ball_x > ball_size(True -> vai para a proxima linha, False-> Vai para a proxima coluna)
	sub ax, box_x
	cmp ax, box_height
	jng draw_boxs
			
	mov cx, box_x       ;O registo CX volta a primeira coluna
	inc dx              ;Avanca uma linha
			
	mov ax, dx		    ; dx - ball_y > size (True -> Sai do procedimento, False -> Vai para a proxima linha)
	sub ax, box_y
	cmp ax, box_width
	jng draw_boxs

    ret
draw_box endp  

;*****************************************************************
; select_menu - funcao responsavel selecionar a opcao do menu
;***************************************************************** 
select_menu proc
    call HideBlinkingCursor
    mov ax, 0 ;mouse ininialization. any previous mouse pointer is hidden. 
    int 33h
    cmp ax, 0FFFFh ;if successful: AX=0FFFFh and BX=number of mouse buttons.
    jne Error
    mov ax, 1 ;show mouse pointer. 
    int 33h
    
    select:
        mov ax, 3 ;get mouse position and status of its buttons.
        int 33h
        cmp bx, 1 ;if left button is down: BX=1
        jne select
        cmp cx, 50h ;verifica se foi selecionado a esquerda do menu  
        jle select
        cmp cx, 0F0h;verifica se foi selecionado a direita do menu 
        jg select
        cmp dx, 30;verifica se foi selecionado acima do menu 
        jle select
        cmp dx, 190;verifica se foi selecionado abaixo do menu 
        jg select
        
        cmp dx, 60
        jle op1
        cmp dx, 70
        jle select
        cmp dx, 100
        jle op2
        cmp dx, 110
        jle select
        cmp dx, 140
        jle op3
        cmp dx, 150
        jle select
        cmp dx, 180
        jle op4
        jmp select 
        
        op1:
            mov cx, 1
            jmp end_select 
        op2:
            mov cx, 2
            jmp end_select
        op3:
            mov cx, 3
            jmp end_select
        op4:
            mov cx, 4
            jmp end_select
            
        error:
            mov cx, 0
            jmp end_select 
            
        end_select:  
            mov ax, 2 ;hide visible mouse pointer
            int 33h
            ret
        
    ret
select_menu endp  

Reg_Score_Player1 proc
    push si
    push ax
    push bx
    push cx
    push dx

        ;Pontuacao
        lea si, str_result 
        
        push 2                                     ;numero de algarismos a converter
        mov al, points1
        mov ah,0                                   
        cbw                                        ;converter um byte para uma word, a funcao fTransformarAscii tem como parametro uma word em ax
        call get_Ascii
        add sp, 4                                  
        
        ;Colocar espacos
        push cx
        mov cx, 5
        Init_space1_p1: cmp cx,0
        je Exit_space1_p1
        mov [si],20h
        inc si
        dec cx 
        jmp Init_space1_p1
        Exit_space1_p1:
        pop cx
        
        ;Nome
        push bx
        push dx
        push cx                                  
        mov cx,12
        lea bx,player1
        copy1: cmp cx,0
        je not_end1                                               
        mov dx,[bx]                                ;Copiar o nome do jogador a registar para a StringResultados
        mov [si],dx                                ;
        inc si
        inc bx
        dec cx
        jmp copy1
        not_end1:
        pop cx
        pop dx
        pop bx
        
        ;Colocar espacos
        push cx
        mov cx, 5
        Init_space2_p1: cmp cx,0
        je Exit_space2_p1
        mov [si],20h
        inc si
        dec cx 
        jmp Init_space2_p1
        Exit_space2_p1:
        pop cx
                
        ;Data
        push 2                 ;numero de algarismos a converter
        mov ah, 2ah
        int 21h
        xor ax, ax
        mov al, dl    ;        ;dl = dia
        call get_Ascii ;Dia
        mov [si], '-'
        inc si
        mov al, dh    ;        ;dh = mes
        call get_Ascii ;Mes
        mov [si], '-'
        inc si
        mov ax, cx             ;cx = ano
        push 4   
        call get_Ascii ;Ano
        add sp, 2
        mov [si],20h           ;Caracter espaco em ASCII
        inc si
        
        ;Hora
        mov ah, 2ch
        int 21h
        xor ax, ax             
        mov al, ch    ;        ;ch = horas
        call get_Ascii ;Horas
        mov [si], ':'
        inc si
        mov al, cl    ;        ;cl = minutos
        call get_Ascii ;Minutos
        mov [si], ':'
        inc si
        mov al, dh    ;        ;dh = segundos
        call get_Ascii ;Segundos
        inc si
        
        ;Escrever os dados no ficheiro 
        lea dx, ResultFile
        mov al, 2
        call FOpen
        jnc Write_File1
        xor cx, cx
        call FCreate
        
        Write_File1:
            mov bx, ax         ;ax = file handler
            xor cx, cx
            xor dx, dx
            mov al, 2 
            call fSeek
            lea dx, str_result
            mov cx, 40
            call fWrite            
        
        call FClose
                   
    pop dx
    pop cx
    pop bx
    pop ax
    pop si
    ret
Reg_Score_Player1 endp
  
Reg_Score_Player2 proc
    push si
    push ax
    push bx
    push cx
    push dx

;Pontuacao
        lea si, str_result2
        push 2                                     ;numero de algarismos a converter
        mov al, points2
        mov ah,0                                   
        cbw                                        ;converter um byte para uma word, a funcao fTransformarAscii tem como parametro uma word em ax
        call get_Ascii
        add sp, 4                                  
        
        ;Colocar espacos
        push cx
        mov cx, 5
        Init_space1_p2: cmp cx,0
        je Exit_space1_p2
        mov [si],20h
        inc si
        dec cx 
        jmp Init_space1_p2
        Exit_space1_p2:
        pop cx
        
        ;Nome
        push bx
        push dx
        push cx                                  
        mov cx,12
        lea bx,player2
        copy2: cmp cx,0
        je not_end2                                               
        mov dx,[bx]                                ;Copiar o nome do jogador a registar para a StringResultados
        mov [si],dx                                ;
        inc si
        inc bx
        dec cx
        jmp copy2
        not_end2:
        pop cx
        pop dx
        pop bx
        
        ;Colocar espacos
        push cx
        mov cx, 5
        Init_space2_p2: cmp cx,0
        je Exit_space2_p2
        mov [si],20h
        inc si
        dec cx 
        jmp Init_space2_p2
        Exit_space2_p2:
        pop cx
                
        ;Data
        push 2                 ;numero de algarismos a converter
        mov ah, 2ah
        int 21h
        xor ax, ax
        mov al, dl    ;        ;dl = dia
        call get_Ascii ;Dia
        mov [si], '-'
        inc si
        mov al, dh    ;        ;dh = mes
        call get_Ascii ;Mes
        mov [si], '-'
        inc si
        mov ax, cx             ;cx = ano
        push 4   
        call get_Ascii ;Ano
        add sp, 2
        mov [si],20h           ;Caracter espaco em ASCII
        inc si
        
        ;Hora
        mov ah, 2ch
        int 21h
        xor ax, ax             
        mov al, ch    ;        ;ch = horas
        call get_Ascii ;Horas
        mov [si], ':'
        inc si
        mov al, cl    ;        ;cl = minutos
        call get_Ascii ;Minutos
        mov [si], ':'
        inc si
        mov al, dh    ;        ;dh = segundos
        call get_Ascii ;Segundos
        inc si
        
        ;Escrever os dados no ficheiro 
        lea dx, ResultFile
        mov al, 2
        call FOpen
        jnc Write_File2
        xor cx, cx
        call FCreate
        
        Write_File2:
            mov bx, ax         ;ax = file handler
            xor cx, cx
            xor dx, dx
            mov al, 2 
            call fSeek
            lea dx, str_result2
            mov cx, 40
            call fWrite            
        
        call FClose
                   
    pop dx
    pop cx
    pop bx
    pop ax
    pop si
    ret
Reg_Score_Player2 endp 
 ;***************************************************************************************
    ;* Transforma um numero para ASCII                                                     *
    ;* Parametros: ax-Numero a transformar si-buffer de destino Stack-Numero de algarismos *
    ;***************************************************************************************
    Get_Ascii proc
        push dx
        push cx
        push bx
        push bp
        
        mov bp, sp
        
        xor bh, bh
        mov bl, 10
        xor cx, cx
        
        GetNum:
            xor dx, dx
    	    div bx
    	    push dx  
        	inc cx
    	    cmp ax, 0
    	    jne GetNum
    	    PutZero:
    	        cmp cx, [bp+10]
    	        je PutNum
    	        push 0
    	        inc cx
    	        jmp PutZero
    	    
    	PutNum:
    	    pop [si]
    	    add [si], 48
    	    inc si
    	    dec cx
    	    or cx, 0
    	    jz EndAscii
    	    jmp PutNum    	    
    	    
    	EndAscii:
    	    pop bp
    	    pop bx
    	    pop cx
    	    pop dx
    	    ret    	            
    Get_Ascii endp
;*****************************************************************
; manage_game - funcao responsavel por gerir as rondas e a vez de jogar do jogo
;***************************************************************** 
manage_game proc
    inicio:
   
        not_e:
        call game 
        mov ah, points
        add points1, ah
        call update_text_player1
  
	 
        mov ax, 00h
        mov points, ah
        mov ax, 00h
        mov enter, ax 
         
        call game 
        mov ah, points
        add points2, ah 
        call update_text_player2
        
        mov ax, 00h
        mov points, ah
        mov ax, 00h
        mov enter, ax 
        
        call update_text_atual_round
        mov al, total_rounds
        cmp atual_round, al
        je exit_manage_game
        inc atual_round 
        jmp inicio
        
        exit_manage_game: 
        
    ret
manage_game endp   
;*****************************************************************
; reset_game- funcao responsavel por restaurar os dados jogo
;***************************************************************** 
reset_game proc
    mov ah, 00h
    mov atual_round, ah 
    mov points1, ah
    mov points2, ah 
    call update_text_player1
    call update_text_player2
    call update_text_atual_round
    mov ax, 2Ch
    mov xx_x, ax
    mov ax, 6Ah
    mov xx_y, ax
    mov ax, 25h
    mov xy_x, ax
    mov ax, 71h
    mov xy_y, ax
    ret
reset_game endp    
;*****************************************************************
; jogo - funcao responsavel por gerir o jogo
;***************************************************************** 
game proc 
     
check_time:               ;loop para verificar o tempo
	mov ah, 2Ch               ;obtem o tempo do sistema
	int 21h                   ;ch = hour cl = minute dh = second dl = 1/100 seconds 
			
	cmp dl, time_aux          ;verifica se o tempo atual e igual ao anterior (time_aux)
	je check_time             ;se for o mesmo verifica novamente
		
    ;Se chegar a este ponto signica que o tempo mudou	
	mov time_aux, dl          ;atualiza o tempo
			
    ;Inicio do desenho ecra de jogo
    mov al, 13h
    call setvideomode
    
    ;verifica se sair foi premido 
               
    ;Desenhar bola
    call draw_main_ball

    ;Desenhar pinos
    call draw_all_pins
    
    ;Desenhar linhas
    mov cx, linha_x
    mov dx, linha1_y
    call draw_line
    
    mov cx, linha_x
    mov dx, linha2_y
    call draw_line
    
    ;Desenhar e movimentar X	
	call move_x       ;funcao que movimenta o X para cima ou para baixo
    call draw_x       ;desenhar o X com as posicoes atualizadas
	
	call draw_trajectory 
    call ball_trajectory 
    call calculate_points
    
	;escrever sair
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,18h                       ;set row 
	MOV DL,00h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, sair2      ;give DX a pointer 
	INT 21h  		
	
	; jogador 1
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,00h                       ;set row 
	MOV DL,01h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,player1     ;give DX a pointer 
	INT 21h                          ;print the string
    
    ;jogador 2
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,03h                       ;set row 
	MOV DL,01h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,player2      ;give DX a pointer 
	INT 21h 
	
	;pontos jogador 1
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,01h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,text_points1     ;give DX a pointer 
	INT 21h                          ;print the string
    
    ;pontos jogador 2
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,04h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,text_points2      ;give DX a pointer 
	INT 21h
	
     ;rondas
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,02h                       ;set row 
	MOV DL,01Ch						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,str_ronda      ;give DX a pointer 
	INT 21h
	
	;ronda atual
	MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,02h                       ;set row 
	MOV DL,023h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,text_atual_round      ;give DX a pointer 
	INT 21h 
	
	;/
	MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,02h                       ;set row 
	MOV DL,025h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,str_div     ;give DX a pointer 
	INT 21h

	; rondas totais
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,02h                       ;set row 
	MOV DL,026h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,text_total_rounds      ;give DX a pointer 
	INT 21h
	
	mov ax,131h 
    cmp ball_x_center, ax
    jg exit_jogo
    mov ax, linha1_y
    cmp ball_y_center, ax
    jl exit_jogo
    mov ax, linha2_y
    cmp ball_y_center, ax
    jg exit_jogo

    jmp check_time    ;Volta a verificar o tempo
    
    exit_jogo:  
    mov ax, ball_original_x_center
    mov ball_x_center, ax
     mov ax, ball_original_y_center
    mov ball_y_center, ax
    ret
game endp   

;*****************************************************************
; draw_main_ball - Funcao responsavel por desenhar a bola
;*****************************************************************
draw_main_ball proc
    
    mov cx, ball_x    ;define a coluna inicial (x)
	mov dx, ball_y    ;define a linha inicial (y)
		
    draw_main:
	call config_red
			
	inc cx            ;cx = cx +1 
	mov ax, cx	      ;cx - ball_x > ball_size(True -> vai para a proxima linha, False-> Vai para a proxima coluna)
	sub ax, ball_x
	cmp ax, ball_main_size
	
	jng draw_main
			
	mov cx, ball_x     ;O registo CX volta a primeira coluna
	inc dx             ;Avanca uma linha
			
	mov ax, dx		   ;dx - ball_y > size (True -> Sai do procedimento, False -> Vai para a proxima linha)
	sub ax, ball_y
	cmp ax, ball_main_size
	jng draw_main

    ret
draw_main_ball endp 

;*****************************************************************
; draw_all_pins- funcao responsavel por desenhar todos os pinos
;*****************************************************************
draw_all_pins proc
    mov cx, pin1_x
    mov dx, pin1_y
    mov pin_x, cx ;usado para colocar o valor do pino 1 na funcao generica
    mov pin_y, dx
    call draw_pin
    
    mov cx, pin2_x
    mov dx, pin2_y 
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin
    
    mov cx, pin3_x
    mov dx, pin3_y
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin
    
    mov cx, pin4_x
    mov dx, pin4_y
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin
    
    mov cx, pin5_x
    mov dx, pin5_y
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin
    
    mov cx, pin6_x
    mov dx, pin6_y
    mov pin_x, cx
    mov pin_y, dx     
    call draw_pin
    
    mov cx, pin7_x
    mov dx, pin7_y
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin 
    
    mov cx, pin8_x
    mov dx, pin8_y
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin
    
    
    mov cx, pin9_x
    mov dx, pin9_y
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin
    
    mov cx, pin10_x
    mov dx, pin10_y
    mov pin_x, cx
    mov pin_y, dx
    call draw_pin 
    ret
draw_all_pins endp
    
;*****************************************************************
; draw_pin - funcao responsavel por desenhar um pino
;*****************************************************************
draw_pin proc
		
    draw_pins:
    call config_white 
			
	inc cx             ; cx = cx +1 
	mov ax, cx	       ; cx - ball_x > ball_size(True -> vai para a proxima linha, False-> Vai para a proxima coluna)
	sub ax, pin_x
	cmp ax, pin_x_size
	jng draw_pins
			
	mov cx, pin_x       ;O registo CX volta a primeira coluna
	inc dx              ;Avanca uma linha
			
	mov ax, dx		    ; dx - ball_y > size (True -> Sai do procedimento, False -> Vai para a proxima linha)
	sub ax, pin_y
	cmp ax, pin_y_size
	jng draw_pins

    ret
draw_pin endp  

;*****************************************************************
; draw_line - funcao responsavel por desenhar uma linha
;*****************************************************************
draw_line proc
    
    draw_l:
	call config_grey
			
	inc cx             ;cx = cx +1 
	mov ax, cx	       ;cx - ball_x > ball_size(True -> vai para a proxima linha, False-> Vai para a proxima coluna)
	sub ax, 140h
	cmp ax, 00h
	jng draw_l
	
    ret
draw_line endp 

;*****************************************************************
; draw_x - funcao responsavel por desenhar o X 
;*****************************************************************
draw_x proc
	mov cx, xx_x       ;define a coluna inicial para a linha horizontal (x)
	mov dx, xx_y       ;define a linha inical para a linha horizontal (y)	
	
	draw_x_v:
	call config_grey
	
	inc cx      
	mov ax, cx	
	sub ax, xx_x
	cmp ax, x_width
	jng draw_x_v
			
	mov cx, xx_x 
	inc dx        
	
	mov ax, dx			
	sub ax, xx_y
	cmp ax, x_height
	jng draw_x_v
	
	mov cx, xy_x      ;define a coluna inicial para a linha vertival (x)
	mov dx, xy_y      ;define a linha inical para a linha vertical (y)	
	
	draw_x_h:
	call config_grey
	
	inc cx      
	mov ax, cx	
	sub ax, xy_x
	cmp ax, x_height
	jng draw_x_h
			
	mov cx, xy_x 
	inc dx         
	
	mov ax, dx				
	sub ax, xy_y
	cmp ax, x_width
	jng draw_x_h		
	
	
    ret
draw_x endp
;*****************************************************************
; draw_exit_box - funcao responsavel por a caixa do comando de sair
;*****************************************************************             
draw_exit_box proc
    
    mov cx, exit_x  ;define a coluna inicial (x)
	mov dx, exit_y    ;define a linha inicial (y)
	
	draw_exit_boxs:	
	call config_white
			
	inc cx            ;cx = cx +1 
	mov ax, cx	      ;cx - ball_x > ball_size(True -> vai para a proxima linha, False-> Vai para a proxima coluna)
	sub ax, exit_x
	cmp ax, exit_height
	
	jng draw_exit_boxs
			
	mov cx, exit_x     ;O registo CX volta a primeira coluna
	inc dx             ;Avanca uma linha
			
	mov ax, dx		   ;dx - ball_y > size (True -> Sai do procedimento, False -> Vai para a proxima linha)
	sub ax, exit_y
	cmp ax, exit_width

	jng draw_exit_boxs 
	
draw_exit_box endp               
;*****************************************************************
; move_x - funcao responsavel pelo movimento do X 
;*****************************************************************
move_x proc 
    
	;Verifica se alguma tecla foi premida 
	mov ah,01h
	int 16h
	jz exit_x_movement ;zf = 1, jz -> salta se for zero
		
	;Verifica que tecla foi premida (al = ascii character)
	mov ah,00h
	int 16h
		
	;se for seta para cima move para cima
	cmp ah,48h 
	je move_x_up
	;se for seta para baixo para baixo
	cmp ah,50h 
	je move_x_down
	
	
	jmp exit_x_movement
		
	move_x_up:
	    mov ax, x_velocity	
		sub xx_y, ax
		sub xy_y, ax
			
		mov ax, linha1_y
		cmp xx_y, ax
		jl fix_x_top_position
		jmp exit_x_movement
			
			
		fix_x_top_position:
			mov ax, linha1_y
			mov xx_y, ax
			add ax, 07h
			mov xy_y, ax 
			jmp exit_x_movement
				
	move_x_down:
		mov ax, x_velocity	
		add xx_y, ax
		add xy_y, ax
			
		mov ax, linha2_y
		sub ax, 0eh
		;sub ax, x_height 
			
		cmp xx_y, ax
		jg fix_x_bottom_position
			
		fix_x_bottom_position:
			mov xx_y, ax 
			add ax, 07h
			mov xy_y, ax
			jmp exit_x_movement
			 		
	exit_x_movement:
			ret
move_x endp
;*****************************************************************
; draw_trajectory - funcao responsavel por desenhar a trajetoria que a bola faz depois de ser lancada
;*****************************************************************
draw_trajectory proc
    mov cx, ball_x_center    ;define a coluna inicial (x)
	mov dx, ball_y_center    ;define a linha inicial (y)
		
	call config_yellow
    ret
draw_trajectory endp    
;*****************************************************************
; ball_trajectory - funcao responsavel por definir a trajetoria da bola
;*****************************************************************
ball_trajectory proc
    
    call check_enter 
    mov ax, 00h
    cmp enter, ax
    jg ball_move
    jmp exit_trajectory
    
    ball_move:
    call calculate_y_velocity
    
    mov ax, ball_x_velocity
    add ball_x_center, ax
    
    move_ball_vertically:
    mov ax, ball_y_velocity
    add ball_y_center, ax
    
    exit_trajectory:		
    ret   
ball_trajectory endp
;*****************************************************************
; calculate_y_velocity - funcao responsavel calcular a a velocidade da bola no eixo dos y
;*****************************************************************
calculate_y_velocity proc
    
    ;calcula a diferenca entre o y da bola e o y do X 
    mov ax, xy_y 
    mov ball_y_velocity, ax
    mov ax, ball_original_y_center
    sub ball_y_velocity, ax
    jmp exit_calculate
   
    exit_calculate:
    ret
calculate_y_velocity endp
;*****************************************************************
; calculate_points - funcao responsavel por verificar se a bola interseta os pinos
;*****************************************************************
calculate_points proc
     ;Check if the ball is colliding 
    ;maxx1 > minx2 && minx1 < maxx2 && maxy1 > miny2 && miny1 < maxy2
    
    mov ax, pin10_x
    cmp ball_x_center, ax
    jl check_collision_pin9
    
    mov ax, pin10_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin9
    
    mov ax, pin10_y 
    cmp ball_y_center, ax 
    jl check_collision_pin9
    
    mov ax, pin10_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin9
    
    inc points
    
    check_collision_pin9:
    mov ax, pin9_x
    cmp ball_x_center, ax
    jl check_collision_pin8
    
    mov ax, pin9_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin8
    
    mov ax, pin9_y 
    cmp ball_y_center, ax 
    jl check_collision_pin8
    
    mov ax, pin9_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin8
    
    inc points
    
    check_collision_pin8:
    mov ax, pin8_x
    cmp ball_x_center, ax
    jl check_collision_pin7
    
    mov ax, pin8_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin7
    
    mov ax, pin8_y 
    cmp ball_y_center, ax 
    jl check_collision_pin7
    
    mov ax, pin8_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin7
    
    inc points
    
    check_collision_pin7:
    mov ax, pin7_x
    cmp ball_x_center, ax
    jl check_collision_pin6
    
    mov ax, pin7_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin6
    
    mov ax, pin7_y 
    cmp ball_y_center, ax 
    jl check_collision_pin6
    
    mov ax, pin7_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin6
    
    inc points
    
    check_collision_pin6:
    mov ax, pin6_x
    cmp ball_x_center, ax
    jl check_collision_pin5
    
    mov ax, pin6_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin5
    
    mov ax, pin6_y 
    cmp ball_y_center, ax 
    jl check_collision_pin5
    
    mov ax, pin6_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin5
    
    inc points
    
    check_collision_pin5:
    mov ax, pin5_x
    cmp ball_x_center, ax
    jl check_collision_pin4
    
    mov ax, pin5_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin4
    
    mov ax, pin5_y 
    cmp ball_y_center, ax 
    jl check_collision_pin4
    
    mov ax, pin5_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin4
    
    inc points
    
    check_collision_pin4:
    mov ax, pin4_x
    cmp ball_x_center, ax
    jl check_collision_pin3
    mov ax, pin4_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin3
    
    mov ax, pin4_y 
    cmp ball_y_center, ax 
    jl check_collision_pin3
    
    mov ax, pin4_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin3
    
    inc points
    
    check_collision_pin3:
    mov ax, pin3_x
    cmp ball_x_center, ax
    jl check_collision_pin2
    
    mov ax, pin3_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin2
    
    mov ax, pin3_y 
    cmp ball_y_center, ax 
    jl check_collision_pin2
    
    mov ax, pin3_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin2
    
    inc points
    
    check_collision_pin2:
    mov ax, pin2_x
    cmp ball_x_center, ax
    jl check_collision_pin1
    
    mov ax, pin2_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg check_collision_pin1
    
    mov ax, pin2_y 
    cmp ball_y_center, ax 
    jl check_collision_pin1
    
    mov ax, pin2_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg check_collision_pin1
    
    inc points
    
    check_collision_pin1:
    mov ax, pin1_x
    cmp ball_x_center, ax
    jl exit_check_collision
    
    mov ax, pin1_x
    add ax, pin_x_size
    cmp ball_x_center, ax
    jg exit_check_collision
    
    mov ax, pin1_y 
    cmp ball_y_center, ax 
    jl exit_check_collision
    
    mov ax, pin1_y
    add ax, pin_y_size
    cmp ball_y_center, ax
    jg exit_check_collision
    
    inc points
    
    exit_check_collision:
    ret
calculate_points endp 
;*****************************************************************
; check_enter - funcao responsavel por verificar se a tecla enter foi premida
;*****************************************************************
check_enter proc
    ;Verifica se alguma tecla foi premida 
	mov ah,01h
	int 16h
	jz exit_enter;zf = 1, jz -> salta se for zero
		
	;Verifica que tecla foi premida (al = ascii character)
	mov ah,00h
	int 16h
		
	;se for enter move para cima
	cmp al, 0Ah ;enter
	je enter_true
	

    enter_true: 
    inc enter
	exit_enter:
    ret
check_enter endp  
;*****************************************************************
; reset_ball_position - funcao responsavel por restaurar a posicao original da bola
;*****************************************************************
reset_ball_position proc
    mov ax, ball_original_x_center
	mov ball_x_center, ax
		
	mov ax, ball_original_y_center
	mov ball_y_center, ax
    ret
reset_ball_position endp 
;*****************************************************************
; reg_palyer1 - funcao responsavel por registar o primeiro jogador
;*****************************************************************                     
reg_player1 proc
    ;Inicio do desenho ecra de jogo
    mov al, 13h
    call setvideomode
        
    ; jogador 1
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,07h                       ;set row 
	MOV DL,03h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,reg1     ;give DX a pointer 
	INT 21h
	 
    mov di, offset player1  
    call scanf       
    
    ret
reg_player1 endp 
;*****************************************************************
; reg_palyer2 - funcao responsavel por registar o segundo jogador
;*****************************************************************  
reg_player2 proc
    ;Inicio do desenho ecra de jogo
    mov al, 13h
    call setvideomode
    
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,07h                       ;set row 
	MOV DL,03h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,reg2     ;give DX a pointer 
	INT 21h 
	
    mov di, offset player2 
    call scanf 

    ret
reg_player2 endp 
;*****************************************************************
; reg_rounda - funcao responsavel por registar o numero de rondas do jogo
;***************************************************************** 
reg_rounds proc
    choose_again:
    
    mov al, 13h
    call setvideomode
    
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,07h                       ;set row 
	MOV DL,03h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, str_rounds     ;give DX a pointer 
	INT 21h 
	
    mov di, offset text_total_rounds
    call scanf
	
	
    mov al, text_total_rounds
    cmp al, 30h
    jl InvalidLevel
    cmp al, 39h
    jg InvalidLevel
   
    jmp ValidOption
    
    InvalidLevel:
       MOV AH,02h                       ;set cursor position
       MOV BH,00h                       ;set page number
	   MOV DH,07h                       ;set row 
	   MOV DL,03h						 ;set column
	   INT 10h							 
		
	   MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	   LEA DX,rounds_inv     ;give DX a pointer 
	   INT 21h  
	   
	   mov str_rounds, 00h
       jmp choose_again
       
       mov ah, 1
       int 21h
       
    validOption:
    
    call update_total_rounds
    ret
reg_rounds endp

;*****************************************************************
;update_text_player1 - funcao responsavel por converter o valor dos pontos do jogador 1 para o seu codigo ASCII
;***************************************************************** 
update_text_player1 proc
    
    xor ax, ax
    mov al, points1
    ;now, before printing to the screen, we need to convert the decimal value to the ascii code character 
		;we can do this by adding 30h (number to ASCII)
		;and by subtracting 30h (ASCII to number)
    add al, 30h
    mov [text_points1], al
    
    ret
update_text_player1 endp   
;*****************************************************************
;update_text_player2 - funcao responsavel por converter o valor dos pontos do jogador 2 para o seu codigo ASCII
;***************************************************************** 		
update_text_player2 proc
    xor ax, ax
    mov al, points2
    ;now, before printing to the screen, we need to convert the decimal value to the ascii code character 
		;we can do this by adding 30h (number to ASCII)
		;and by subtracting 30h (ASCII to number)
    add al, 30h
    mov [text_points2], al
    
    ret
update_text_player2 endp 
;*****************************************************************
;update_text_atual_round - funcao responsavel por converter a ronda atual do seu codigo ASCII
;*****************************************************************     
update_text_atual_round proc
    xor ax, ax
    mov al, atual_round
    ;now, before printing to the screen, we need to convert the decimal value to the ascii code character 
		;we can do this by adding 30h (number to ASCII)
		;and by subtracting 30h (ASCII to number)
    add al, 30h
    mov [text_atual_round], al
    
    ret
update_text_atual_round endp 

;*****************************************************************
;update_total_rounds - funcao responsavel por converter a ronda atual para o seu codigo ASCII
;*****************************************************************         	
update_total_rounds proc
    xor ax, ax
    mov al, text_total_rounds
    
    sub al, 30h
    mov [total_rounds], al
    ret
update_total_rounds endp

;*****************************************************************
;draw_winner1- funcao responsavel por apresentar a tela que informa que o jogador 1 venceu o jogo
;*****************************************************************     
draw_winner1 proc 
    mov al, 13h
    call setvideomode
        
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,07h                       ;set row 
	MOV DL,04h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,text_winner     ;give DX a pointer 
	INT 21h
	
	MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,0Ah                       ;set row 
	MOV DL,04h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,player1     ;give DX a pointer 
	INT 21h
	
	mov ah, 1
	int 21h
    ret
draw_winner1 endp 

;*****************************************************************
;draw_winner2- funcao responsavel por apresentar a tela que informa que o jogador 2 venceu o jogo
;*****************************************************************     
draw_winner2 proc 
    mov al, 13h
    call setvideomode
        
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,07h                       ;set row 
	MOV DL,05h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,text_winner     ;give DX a pointer 
	INT 21h
	
	MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,0Ah                       ;set row 
	MOV DL,05h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,player2     ;give DX a pointer 
	INT 21h     
	
	mov ah, 1
	int 21h
    ret
draw_winner2 endp 

;*****************************************************************
;draw_draw- funcao responsavel por apresentar a tela que informa que os jogadores empataram
;*****************************************************************     
draw_draw proc 
    mov al, 13h
    call setvideomode
        
	MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,0Ah                       ;set row 
	MOV DL,05h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,text_draw     ;give DX a pointer 
	INT 21h     
	
	mov ah, 1
	int 21h
    ret
draw_draw endp

;*****************************************************************
;write_creditos - funcao responsavel por escrever na tela os creditos
;*****************************************************************     
write_creditos proc
    ;escrever Trabalho realizado
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,02h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX,TR      ;give DX a pointer 
	INT 21h 
	
	;escrever Nome 1
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,04h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, nome1      ;give DX a pointer 
	INT 21h   
	
	;escrever Nome 2
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,06h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, nome2      ;give DX a pointer 
	INT 21h   
	
	;escrever Nome 3
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,08h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, nome3      ;give DX a pointer 
	INT 21h 
	
	;escrever Docentes
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,0Bh                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, docentes      ;give DX a pointer 
	INT 21h 
	
	;escrever docente1
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,0Dh                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, doc1      ;give DX a pointer 
	INT 21h 
	
	;escrever docente 2
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,0Fh                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, doc2      ;give DX a pointer 
	INT 21h 
	
	;escrever docente 3
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,11h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, doc3      ;give DX a pointer 
	INT 21h 
	
	;escrever docente 4
    MOV AH,02h                       ;set cursor position
	MOV BH,00h                       ;set page number
	MOV DH,13h                       ;set row 
	MOV DL,02h						 ;set column
	INT 10h							 
		
	MOV AH,09h                       ;WRITE STRING TO STANDARD OUTPUT
	LEA DX, doc4     ;give DX a pointer 
	INT 21h 

	mov ah, 1
    int 21h
        
    ret
write_creditos endp
                                   
;*****************************************************************
; config_white - define as condicoes necessarias para pintar um pixel de branco
;*****************************************************************
config_white proc
	mov ah, 0Ch        ;configurar para escrever um pixel
	mov al, 0Fh        ;escolha a cor da bola como branco
	mov bh, 00h        ;escolhe o numero da pagina
	int 10h            ;executa as configuracoes 
	ret
config_white endp 

;*****************************************************************
; config_red - define as condicoes necessarias para pintar um pixel de vermelho 
;*****************************************************************
config_red proc
	mov ah, 0Ch        ;configurar para escrever um pixel
	mov al, 0Ch        ;escolha a cor da bola como vermelho
	mov bh, 00h        ;escolhe o numero da pagina
	int 10h            ;executa as configuracoes 
	ret
config_red endp 

;*****************************************************************
; config_grey - define as condicoes necessarias para pintar um pixel de cinzento
;*****************************************************************
config_grey proc
	mov ah, 0Ch        ;configurar para escrever um pixel
	mov al, 07h        ;escolha a cor da bola como cinzento
	mov bh, 00h        ;escolhe o numero da pagina
	int 10h            ;executa as configuracoes 
	ret
config_grey endp 

;*****************************************************************
; config_yellow - define as condicoes necessarias para pintar um pixel de amarelo
;*****************************************************************
config_yellow proc

	mov ah, 0Ch        ;configurar para escrever um pixel
	mov al, 0Eh        ;escolha a cor da bola como amarelo
	mov bh, 00h        ;escolhe o numero da pagina
	int 10h            ;executa as configuracoes 
	ret
config_yellow endp 

; ****************************************************
; Set Video Mode
;
; Input:
; 	AL- Video Mode
;		- 00h - text mode. 40x25. 16 colors. 8 pages.
;		- 03h - text mode. 80x25. 16 colors. 8 pages.
;		- 13h - graphical mode. 40x25. 256 colors. 320x200 pixels, 1 page. 
; Output:Nothing
; Detroys: Nothing
setVideoMode proc
	push ax
	mov ah,00 ; modo vídeo
	Int 10h
	pop ax
	ret
endp 

; *******************************************
; Set Text Cursor
;
; Input:
; 	CH – Cursor start line (bits 0-4)
;            Opções (bits 5-7)
;	CL – Bottom Cursor (bits 0-4)
;	Bit 5 de CH a 1 esconde cursor
; Output:Nothing
; Detroys: Nothing
HideBlinkingCursor proc
	push cx
	mov ch, 32  ;(0001 0000B)
	mov ah, 1 
	int 10h 
	pop cx
       ret
endp
ShowStandardCursor proc
	push cx
	mov ch, 6 
	mov cl, 7
	mov ah, 1
	int 10h
	pop cx
       ret
endp

ShowBoxCursor proc
	push cx
	mov ch, 0 
	mov cl, 7
	mov ah, 1
	int 10h
	pop cx
       ret
endp
     
clear_screen proc
    push ax
    
    mov ah, 07h
    xor bh, bh
    xor al, al
    xor cx, cx
    mov dh, 25
    int 10h
    
    pop ax
    ret
clear_screen endp

;*****************************************************************
; scanf - string input
; descricao: rotina que faz o input de uma string terminada em Enter a partir do teclado
; input - si=deslocamento da string a escrever desde o início do segmento de dados
; output - nenhum
; destroi - al, si
;*****************************************************************
scanf proc
L2: call ci
    mov [di], al
    inc di
    cmp al, 0dh; compare enter
    je fimscnstr
    jmp L2
    
    fimscnstr: mov [di], '$'
    ret
scanf endp 


;*****************************************************************
; ci - caracter input
; descricao: rotina que faz o input de um caracter para o ecra
; input - 
; output - 
; destroi - 
;*****************************************************************
ci proc
   mov ah, 07h
   int 21h
   ret
ci endp  

  
printstr proc
        push bp
        mov bp, sp
        
        mov bp, [bp+4]   
        mov al, 1 
        mov bh, 0
        mov ah, 13h
        int 10h  
        
        pop bp
        ret 2
endp printstr 

   setcursor proc
        mov bh, 0
        mov ah, 2
        int 10h
        ret
    endp setcursor
    
;*****************************************************************
; printf - string output
; descricao: rotina que faz o output de uma string NULL terminated para o ecra
; input - si=deslocamento da string a escrever desde o início do segmento de dados 
; output - nenhum
; destroi - al, si
;*****************************************************************
printf proc
L1: 	mov al,byte ptr [si]
    or al,al
    jz fimprtstr
    call co
    inc si
    jmp L1
fimprtstr: ret
printf endp 

;*****************************************************************
; co - caracter output
; descricao: rotina que faz o output de um caracter para o ecra
; input - al=caracter a escrever
; output - nenhum
; destroi - nada
;*****************************************************************
co proc
    push ax
    push dx
    mov ah,02H
    mov dl,al
    int 21H
    pop dx
    pop ax
    ret
co endp 

;*****************************************************************
;clear_buffer - funcao responsavel por limpar o buffer
;*****************************************************************     
Clear_Buffer proc
        push ax
        
        mov ah, 0ch
        xor al, al
        int 21h
            
        pop ax
        ret
Clear_Buffer endp
    
;**************************************** Funcoes de Ficheiros ***********************************************
  ;*****************************************************************************
    ;* Cria um ficheiro                                                          *
    ;* Parametros: dx-Nome, cx-Atributos                                         *
    ;* Retorno: CF-Sucesso/Insucesso(0/1), ax-Handler(CF=0)/Codigo de Erro(CF=1) *
    ;*****************************************************************************
    FCreate proc
       
        mov ah, 3Ch
        int 21h
        
        ret    
    FCreate endp
    
    ;*****************************************************************************
    ;* Abre um ficheiro                                                          *
    ;* Parametros: dx-Nome, al-Tipo de acesso(0-R, 1-W, 2-R/W)                   *
    ;* Retorno: CF-Sucesso/Insucesso(0/1), ax-Handler(CF=0)/Codigo de Erro(CF=1) *
    ;*****************************************************************************
    FOpen proc
        
        mov ah, 3Dh
        int 21h        
        
        ret
    FOpen endp
    
    ;***************************************************************
    ;* Le o numero de bytes pedidos de um ficheiro para um buffer  *                
    ;* Parametros: bx-Handler, cx-Numero de bytes a ler, dx-Buffer *           
    ;***************************************************************
    FRead proc
                        
        mov ah, 3Fh
        int 21h
        inc dx
               
        ret    
    FRead endp
    
    ;********************************************************************
    ;* Escreve o numero de bytes pedidos num ficheiro de um buffer      *                
    ;* Parametros: bx-Handler, cx-Numero de bytes a escrever, dx-Buffer *           
    ;********************************************************************    
    FWrite proc
        
        mov ah, 40h
        int 21h
        
        ret    
    FWrite endp
    
    ;**************************
    ;* Fecha o ficheiro       *            
    ;* Parametros: bx-Handler *           
    ;**************************
    FClose proc
        
        mov ah, 3Eh
        int 21h
        
        ret        
    FClose endp
    
    ;*************************************************
    ;* Alterar posicao no ficheiro                   *
    ;* Parametros: al-origem(0-SOF;2-EOF) bx-handler *
    ;*************************************************
    FSeek proc
        push cx
        push dx
        
        xor cx, cx
        xor dx, dx
        mov ah, 42h
        int 21h
        
        pop dx            
        pop cx
        ret
    FSeek endp
         
end start ; set entry point and stop the assembler.
