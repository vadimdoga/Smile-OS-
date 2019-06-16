# Smile-OS :smile:
This is a laboratory work for University course.
## Getting Started
These instructions will get you a copy of the project up and running on your local machine.
### Prerequisites
1. Install bash if you have Windows OS.
2. Install nasm.
```
sudo apt-get install nasm
```
3. Install Virtual Box.
4. Create a Virtual Machine with type Other and version DOS.
5. Compile the code in terminal.
```
nasm -f bin kernel00.asm -o kernel00.bin
nasm -f bin boot.asm -o boot.bin
date > date.bin
printf "Time and Date:" > text.bin
cat text.bin date.bin > about.bin
truncate -s 205312 boot.bin
cat boot.bin boot.bin > boot1.bin
truncate -s 205824 boot1.bin
cat boot1.bin about.bin > file.bin
truncate -s 206336 file.bin
cat file.bin kernel00.bin > disk.img
truncate -s 1474560 disk.img
```
6. Choose floppy image disk.img.
<br/>Have fun :v:

