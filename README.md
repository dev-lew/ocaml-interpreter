# ocaml-interpreter
Credit to the CS320 course staff for the grammar design!
**Do not cheat. If you can find this code, so can your professors and classmates**

Supports the following grammar (in extended BNF):

digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

letter ::= a-z | A-Z

int ::= [−]digit{digit}

bool ::= <true> | <false>
  
name ::= letter{letter|digit|_| ́}

string ::= "{ ASCII\"}"

const ::= int | bool | string | name | <unit>
  
prog ::= coms

com ::=   Push const

        |Pop
        
        |Swap
       
        |Log
        
        |Add
        
        |Sub
        
        |Mul
        
        |Div
        
        |Rem
        
        |Neg
        
        |Cat
        
        |And
        
        |Or
        
        |Not
        
        |Eq
        
        |Lte
        
        |Lt
        
        |Gte
        
        |Gt
        
        |Let
        
        |Ask
        
        |Begin coms End
        
        |If coms Else coms End
        
        |DefFun name name coms End
        
        |Call
        
        |Throw
        
        |Try coms Catch coms End


mem ::= {name, val;}

value ::= int | bool | string | unit | name | DefFun

**Error Codes:** 0, no error. 1, type error. 2, too few elements on stack. 3, div by 0. 4, var not in scope. i, user defined error.



Command operational semantics are fairly self explanatory, but to explain a few:

**Let** - Takes a name and value from the stack and binds the name to that value (stores it in the enviroment)

**Ask** - Takes a name from the stack and pushes its associated value to the stack

**Call** - Takes a name and function value from the stack, and executes the commands in the function body. Pushes top element of the inner stack to the calling stack

