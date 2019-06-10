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
nasm -f bin kernel.asm -o kernel.bin
nasm -f bin boot.asm -o boot.bin
cat boot.bin kernel.bin > disk.img
truncate -s 1474560 disk.img
```
6. Choose floppy image disk.img.
<br/>Have fun :v:

