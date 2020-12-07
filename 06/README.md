Tested on WSL2 x64 Ubuntu
```
nasm -f elf64 part1.asm && ld part1.o -o part1 && ./part1 <input
```
Make sure input uses unix line endings and ends with a blank line