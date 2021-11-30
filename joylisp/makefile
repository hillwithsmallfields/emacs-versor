# Time-stamp: <2021-11-30 21:13:55 jcgs>

all:	joylisp xjoylisp

joylisp: joylisp.c
	cc -g -Wall -o joylisp joylisp.c -lm
	cp joylisp ~/bin

joylisp_graphics.o: joylisp_graphics.c joylisp.h gamepad_diagram.h
	cc -g -DDIAGRAM -Wall -c -o joylisp_graphics.o joylisp_graphics.c

xjoylisp: joylisp.c joylisp_graphics.o joylisp.h
	cc -g -DDIAGRAM -Wall -o xjoylisp joylisp.c joylisp_graphics.o -lm -lX11 -lXt
	cp xjoylisp ~/bin
