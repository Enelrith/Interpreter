Resonance (hence the .rsn file extension) supports the following operations:

`PUSH <number>`: Push a number onto the stack.
`POP`: Pop the top number from the stack.
`ADD`: Pop the top two numbers from the stack, add them, and push the result.
`SUB`: Pop the top two numbers from the stack, subtract them, and push the result.
`MUL`: Pop the top two numbers from the stack, multiply them, and push the result.
`DIV`: Pop the top two numbers from the stack, divide them, and push the result.
`PRINT "<string>"`: Print a string literal.
`READ`: Read a number from the user and push it onto the stack.
`CHOICE`: Enter a loop where the user can choose an operation to perform.
`JUMP.EQ.0 <label>`, `JUMP.GT.0 <label>`, `JUMP.LT.0 <label>`: Conditional jumps to a label if the top of the stack is equal to, greater than, or less than zero.
`PRINT "<string>"`: Print a string.
`PRINT $<variable>`: Print the value of a variable.
`CONCAT <concat_string_name> <string_1_name> <string_2_name>`: Concatenate 2 strings into 1 bigger string.
`WAIT`: Stops executing code until user input.
`HALT`: Stop the program.

You can find example programs in the examples directory. To run an example, use the following command: `python interpreter.py` and enter the file path of the program your want to run (ex. `./examples/Calculator.rsn`).
`!!!` If you are running the interpreter using the .exe and want to run the example programs then instead of `./examples` you need to use `../examples/` `!!!`
