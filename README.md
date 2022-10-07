#TIPA – The Imperative Programming Astonishing Language
##Author: Bartosz Ruszewski

It’s a simple imperative programming language, mostly based on a simple language [Latte](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2011/Latte/).

TIPA programs are interpreted as a list of functions, there has to be a function named main that return int, program will start executing by calling the main().
It’s possible to make a recursive call from any named function, we return a value simply by calling a ‘return’ instruction. Functions could be declared in any block, and they will be visible for any instruction in current and nested blocks. Functions in TIPA are also a valid variable type, so it’s possible to return a function, or pass it as an argument. It’s possible to create anonymous functions.
You can specify if the argument should be passed by value, or a reference by using ‘ref’.

Every function in TIPA must declare it’s body, which is build using a block. Blocks are built of a list of instructions, every instruction is separated by ; TIPA implements all the instructions from Latte + 
for loop, break, continue, print, and if(){} elif() {} else{} construction.

Variable and function declarations shadow the previous declarations, but it’s not possible to declare two variables with the same name in the same block.

TIPA extends the Latte variable types by adding function variable type (as mentioned earlier), and a special python-like tuple type. A tuple length is not limited, and they can store any type. There’s also a mechanism that allows to ‘unpack’ the tuple and assign it’s values to some given variables.

There will be a type checker, that will check if the types in the program match correctly.

All the errors that occurr during the program run, will be handled and an information explaining the error will be provided.

There are example programs provided, which demonstrates some of the TIPA mechanisms.