## Lab 1 : Simple sorting

CSU22014 labs involve C programming on Unix-like systems, such as Linux. All of the lab work will be tested on macneill.scss.tcd.ie, which runs Linux,
so you should make sure that you can access that machine.

### Linux home machine
If you are already on a Linux machine, this is relatively simple.  Your first task is to open a
terminal emulator program on the Linux machine. A terminal is a program that allows you to type Unix commands and run programs. Not every version
of Linux opens a terminal automatically when you log in, and unfortunately different versions of Linux use different terminal programs, such as xterm,
konsole, terminator, or gnome-terminal.

On many versions of Linux typing Alt-F2 will bring up a search mini window at the top of the screen. Type "terminal" into that prompt,
and (hopefully) you will be given a range of terminal programs to choose from, such as konsole, gnome-terminal, terminator, or xterm. If you
select one of these then a terminal window should appear where you can start work.

Once you have opened a terminal, everything becomes very simple. To connect to macneill use the secure shell program ssh by typing:
`ssh -X myusername@macneill.scss.tcd.ie`

### Apple Mac
If you are using an Apple Mac, things are almost as simple. The operating system (OS) used by default on Macs is a Unix-like operating system, although
it's not Linux. Open up a terminal and you can type Unix commands such as ssh.

### Windows
If you are using a Windows machine, it's a little bit more complicated. One simple solution is to use the Windows 10 powershell. Search for powershell in the search box on the
bottom left of the Windows screen. Once you have the powershell open, you can use ssh to connect to macneill. Type:
`ssh myusername@macneill.scss.tcd.ie`

This should bring you directly to macneill, where you can use all the normal Unix commands.

Another Windows option is to use a terminal emulation program such as putty. Putty is not included in Windows by default, but it is free and easy to download. Putty allows you
to open an ssh session to macneill.

A much more sophisticated alternative to putty is MobaXterm, which is more complicated but has the really nice feature that you can open
x-windows sessions. In an x-windows session you can run a program that opens a new window on the remote Linux machine, but the window appears on your screen. To do this,
start MobaXterm, click on start a local terminal, and once you are there type:
`ssh -X myusername@macneill.scss.tcd.ie`

It's also possible to install Linux on your Windows machine by installing the "Linux Subsystem for Windows" on your Windows machine. 


### Once you are on macneill
Once you have connected to macneill, you can use normal Unix commands. If you haven't used a Unix-like OS before, then you're in for a treat, because
Unix is great. To see the files in your current director type:
`ls`
Create a directory for cs2014 lab work:
`mkdir cs2014`
Now switch to that directory:
`cd cs2014`
Create a directory for your first lab:
`mkdir lab1`
Now switch to that directory
`cd lab1`

Download the following files to that directory.

http://www.cs.tcd.ie/David.Gregg/cs2014/labs/sorts.c
http://www.cs.tcd.ie/David.Gregg/cs2014/labs/numbers.txt

On many (but not all) Unix machines you can use the wget command to download files. For example:
`wget http://www.cs.tcd.ie/David.Gregg/cs2014/labs/sorts.c`


There are two important types of remote connections to Linux machines: connections with a simple terminal emulator, or connections with an
X-windows client. If you have a simple terminal emulator, then you will need to use editors that run inside a terminal emulator, such as nano
or vim. In this case, the editor takes over your terminal, and you will probably need to open a second terminal for typing commands. But
if you are working with an X-windows client, then you can use editors like gedit and emacs that run within x-windows. If you have an x-session
open, then to use gedit type:
`gedit sorts.c &`

The & at the end of the command tells the terminal to run the gedit program, but not to wait for it to complete. This allows you to
type more commands before the gedit command completes. This is what you want on a Linux machine running X-windows.

If you are running your terminal from a machine that is not running X-windows, then you probably want to run your editor inside the
terminal program. In this case, you would use a simpler editor such as nano or vim, which runs inside the terminal.
`nano sorts.c`

Note that when you are using a terminal editor like nano, you don't want to have the & character at the end of the command,
because the editor is running within the terminal. In this case you will need a second terminal to compile and run your
program.

To compile your program type one of the following:
`clang -o sorts sorts.c`
`gcc -o sorts sorts.c`
`cc -o sorts sorts.c`

These are various C compilers. The standard C compiler on Unix systems is called cc. Gcc is the GNU C compiler. On most Linux systems, the
gcc compiler is the standard compiler, so cc is just another name for gcc. Clang is a relatively new C compiler. Its error messages are
usually much easier to understand than the error messages in gcc. Typically gcc is installed on almost any Unix system, whereas
clang is rarer. However, clang is becoming increasingly popular because its error messages are so much better than gcc's.

To run your program type:
`./sorts`

The file 'sorts.c' contains a simple program to read in some numbers
from a text file, sort them into increasing order, and write them to
the screen. The file 'numbers.txt' contains some numbers to sort.

The existing sorts.c program uses the "funny" sort algorithm to sort the numbers. In addition there are prototypes, but no code for bubble
sort, selection sort and insertion sort. Write the code for these sorts, and test it out in this program. For selection sort, you can
use the existing findMin function (if you want).

Next modify each sorting function so that it returns the number of times that the inner loop of the sort is executed. Modify the main()
function so that it sorts the same original array with each sorting algorithm, and writes out the number of iterations used by each
algorithm.

