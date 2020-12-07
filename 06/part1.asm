; file descriptors
%define STDIN  0
%define STDOUT 1

; syscalls
%define SYS_READ   0
%define SYS_WRITE  1
%define SYS_MMAP   9
%define SYS_MUNMAP 11
%define SYS_EXIT   60

; dedicating a register to the buffer
%define buffer         r15
%define max_buffer_len 5000h

%define ANSWERED 88
%define UNANSWERED 111
%define LETTER_START 97
%define LETTER_END 122

section .data
num_buffer:     db 0, 0, 0, 0, 0, 0, 0, 0
num_buffer_len: equ $ - num_buffer
ans_buffer:     db 'oooooooooooooooooooooooooo', 0Ah
ans_buffer_len: equ $ - ans_buffer
error_msg:      db 'ERROR', 0Ah
error_len:      equ $ - error_msg

section .text
global _start

_start:
  ; read the input into a mmap'd buffer
  call alloc_buffer
  call read_file
  ; read_file stored the length in rax
  call run

  ; Write the number
  call print_number
  mov rdx, rax
	mov	eax, SYS_WRITE
	mov	edi, STDOUT
  lea rsi, [rel num_buffer]
  syscall

  call free_buffer
	mov	eax, SYS_EXIT
  xor rdi, rdi
  syscall

alloc_buffer:
  ; Allocate some space with mmap
  mov eax, SYS_MMAP
  xor rdi, rdi      ; addr is null
  mov rsi, max_buffer_len
  mov rdx, 3        ; PROT_READ | PROT_WRITE
  mov r10, 22h      ; MAP_ANONYMOUS | MAP_PRIVATE
  mov r8, -1        ; set fd to -1 for portability
  syscall
  test rax, rax     ; if the result is negative, error
  js error

  mov buffer, rax
  ret

free_buffer:
  mov eax, SYS_MUNMAP
  mov rdi, buffer
  mov rsi, max_buffer_len
  syscall
  test rax, rax
  jnz error
  ret

read_file:
  mov r14, buffer ; r14 is current pointer
_do_read:
  mov eax, SYS_READ
  mov rdi, STDIN
  mov rsi, r14
  mov rdx, max_buffer_len
  syscall

  test rax, rax
  js error      ; negative result means badness
  jz _done_read ; 0 result means doneness

  ; anything else means try again
  add r14, rax
  jmp _do_read

_done_read:
  sub r14, buffer
  mov rax, r14
  ret

print_number: ; number is in eax
  ; get a pointer to the end of our buffer
  lea rsi, [rel num_buffer]
  ; track the offset of the result string in r13
  mov r13, 7
_print_digit:
  mov edx, 0
  mov ecx, 10
  div ecx ; stores ecx / 10 in eax, ecx % 10 in edx
  add edx, 48 ; convert remainder to ascii
  mov [rsi+r13], dl ; store lower byte in rsi
  sub r13, 1
  test eax, eax
  jnz _print_digit

  ; return the length of the string
  add r13, 1
  mov rax, 8
  sub rax, r13

  ; we have a string at the end of the buffer
  ; copy it to the start
  mov r12, rax
_copy_digit:
  mov dl, [rsi+r13]
  mov [rsi], dl
  add rsi, 1
  sub r12, 1
  jnz _copy_digit

  ret

run: ; eax is the size of the input
  xor r8, r8 ; answer goes in r8
  mov rsi, buffer
  lea rdi, [rel ans_buffer]
  mov r10, rax
  xor rax, rax
_read_char:
  ; read input, decrement counter
  mov al, [rsi]
  and rax, 0x7f
  add rsi, 1
  sub r10, 1

  cmp al, 0Ah
  je _newline
_handle_letter:
  cmp al, LETTER_START
  jb error
  cmp al, LETTER_END
  ja error
  ; it's a valid char, convert it to an offset
  sub al, LETTER_START
  mov byte [rdi + rax], ANSWERED
  jmp _read_char
_newline:
  ; if there is no more input, we are done
  test r10, r10
  jz _group_done

  ; read input, decrement counter
  mov al, [rsi]
  and rax, 0x7f
  add rsi, 1
  sub r10, 1

  cmp al, 0Ah
  jne _handle_letter
_group_done:
; debug: print the line
%if 0
  mov r9, rsi
  mov eax, SYS_WRITE
  mov edi, STDOUT
  lea rsi, [rel ans_buffer]
  mov rdx, ans_buffer_len
  syscall
  mov rdi, rsi
  mov rsi, r9
%endif
xor rcx, rcx
_group_loop:
  mov al, [rdi + rcx]
  and rax, 0x7f
  cmp al, ANSWERED
  jne _group_loop_next
  add r8, 1
_group_loop_next:
  mov byte [rdi + rcx], UNANSWERED
  add rcx, 1
  cmp rcx, 26
  jne _group_loop
  ; if we have input left, keep reading
  test r10, r10
  jnz _read_char
  ; otherwise, we're done!
  mov rax, r8
  ret

error: ; assume error code is in RAX
  mov r14, rax
  ;print error message
	mov	eax, SYS_WRITE
	mov	edi, STDOUT
  lea rsi, [rel error_msg]
  mov rdx, error_len
  syscall

  mov rdi, r14
  ; if it is negative, negate it
  test rdi, rdi
  jns _exit
  neg rdi

_exit:
	mov	eax, SYS_EXIT
  syscall
