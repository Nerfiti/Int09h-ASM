.286
.model tiny
.code
org 100h

;=========================================================================
; Exit
;-------------------------------------------------------------------------
; Entry: None
; Exit : N/A
; Destr: N/A
;-------------------------------------------------------------------------

RES_EXIT    macro

            nop
			mov ax, 3100h
            mov dx, offset EOP
            shr dx, 4
            inc dx
			int 21h
            nop
            
			endm

;=========================================================================
;=========================================================================
;=========================================================================

Start:		cli
            xor bx, bx
            mov es, bx
            mov bx, 4d*9d
        
            mov ax, es:[bx]
            mov OldOffset, ax
            mov ax, es:[bx+2]
            mov OldSeg, ax

            mov ax, offset New09
            mov es:[bx], ax
            mov ax, cs
            mov es:[bx+2], ax
            sti

            mov ax, 3100h
            mov dx, offset EOP
            shr dx, 4
            inc dx		
            int 21h


New09		proc
            push ax cx dx si ds es di
            pushf 		
            
            mov ah, 02h             ;-----------------------------
            int 16h                 ;
            cmp al, 04h	            ; ctrl-flag
            jne Std09               ;-----------------------------
                        
            in al, 60h              ;-----------------------------
            cmp al, 13h             ; key R
            jne Std09               ;-----------------------------

            in al, 61h              ;-----------------------------
            or al, 80h              ;
            out 61h, al             ;
            and al, not 80h         ; Talk with controller
            out 61h, al             ;
                                    ;
            mov al, 20h		        ;
            out 20h, al             ;-----------------------------

                                    ;-----------------------------
            mov ah, 4Eh             ; Main part
            mov cx, 17              ; Set height
            mov dx, 14              ; Set width
            mov si, offset STYLE    ; Set style
            mov bx, cs              ;
            mov ds, bx              ;
            mov bx, 0B800h          ;;
            mov es, bx              ;; Set position
            mov di, 0               ;;
                                    ;
            call DrawFrame          ;
                                    ;-----------------------------
            
            popf
            pop di es ds si dx cx ax
            iret

Std09: 		popf
            pop di es ds si dx cx ax
		    db 0EAh
OldOffset	dw 0
OldSeg		dw 0
		    endp		

;=========================================================================
;=========================================================================
;=========================================================================
; Draw frame
;-------------------------------------------------------------------------
; Entry: AH = attr, CX = height (without up and down), DX = width, DS:SI = style_ptr, ES:DI = position
; Exit : DI = DI + width*height, push ptr to start space in the frame 
; Assum: ES = 0B800h
; Destr: AL, BX, SI, DI
;-------------------------------------------------------------------------

DrawFrame	proc

            mov bx, cx              ; save CX
            mov cx, dx              ; 

            call DrawLine

            mov cx, bx              ;return CX
            
            add di, 80d             ;do offset to the
            sub di, dx              ;   next line

            cmp cx, 0
            je @@LOOP_END

            inc di
            pop bx
            push di
            push bx
            dec di

@@Next:     mov bx, cx              ;    <-------
            mov cx, dx              ;            |
            call DrawLine           ;            |
            mov cx, bx              ;            |  loop
            sub si, 3               ;            |
            add di, 80              ;            |
            sub di, dx              ;            |
            loop @@Next             ;    --------

@@LOOP_END:
            add si, 3
            mov bx, cx
            mov cx, dx
            call DrawLine
            mov cx, bx

			ret
			endp

;=========================================================================
; Draw line
;-------------------------------------------------------------------------
; Entry: AH = attr, SI = style_ptr, CX = width, DI = position (x2 for attr)
; Exit : DI = DI + CX, CX = 0
; Assum: ES = 0B800h
; Destr: AL, CX, DI, SI
;-------------------------------------------------------------------------

DrawLine	proc
            
            shl di, 1

            lodsb
            stosw

            sub cx, 2
            lodsb
            rep stosw

            lodsb
            stosw

            shr di, 1

			ret
			endp

;=========================================================================
;=========================================================================


.data
DrawBuff        db 2*80*25 DUP(0)
SaveBuff        db 2*80*25 DUP(0)

STYLE           db 0C9h, 0CDh, 0BBh, 0BAh, ' ', 0BAh, 0C8h, 0CDh, 0BCh


EOP:
end         Start