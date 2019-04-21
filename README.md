# Smile-OS
## Instructions
To use it you need to have installed nasm on bash if you have windows. After that you need Virtual Box to be installed and create a Virtual Machine. It need to have type Other and version DOS. To compile it you need to introduce next lines in bash:
nasm -f bin kernel.asm -o kernel.bin
nasm -f bin boot.asm -o boot.bin
cat boot.bin kernel.bin > disk.img
truncate -s 1474560 disk.img
After that in setting choose floppy image disk.img.
Have fun!

