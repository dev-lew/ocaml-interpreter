# ocaml-interpreter
Credit to the CS320 course staff for the grammar design!




**Error Codes:** 0, no error. 1, type error. 2, too few elements on stack. 3, div by 0. 4, var not in scope. i, user defined error.



Command operational semantics are fairly self explanatory, but to explain a few:

**Let** - Takes a name and value from the stack and binds the name to that value (stores it in the enviroment)

**Ask** - Takes a name from the stack and pushes its associated value to the stack

**Call** - Takes a name and function value from the stack, and executes the commands in the function body. Pushes top element of the inner stack to the calling stack

