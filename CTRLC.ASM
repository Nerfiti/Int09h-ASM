.286
.model tiny
.code
org 100h

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
		push ax		
		
		mov ah, 02h
		int 16h
		cmp al, 04h	; ctrl-flag
		jne Std09
					
		in al, 60h
		cmp al, 2Eh 	; key C
		jne Std09

		in al, 61h
		or al, 80h
		out 61h, al
		and al, not 80h
		out 61h, al

		mov al, 20h		
		out 20h, al

		mov ax, 4C00h
		int 21h

		pop ax
		iret

Std09: 		pop ax
		db 0EAh
OldOffset	dw 0
OldSeg		dw 0
		endp
EOP:		
end 		Start