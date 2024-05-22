program_lines = []
while True:
    try:
        program_filepath = input("Enter the file path of the program you want to run: ")
        with open(program_filepath, "r") as program_file:
            program_lines = [
                line.strip() 
                    for line in program_file.readlines()]
            break
    except FileNotFoundError:
        print(f"Path not found: {program_filepath}")

program = []
token_counter = 0
label_tracker = {}
variables = {}
for line in program_lines:
    parts  = line.split(" ")
    opcode = parts[0]

    if opcode == "":
        continue

    if opcode.endswith(":"):
        label_tracker[opcode[:-1]] = token_counter
        continue

    program.append(opcode)
    token_counter += 1

    if opcode == "PUSH":
        number = int(parts[1])
        program.append(number)
        token_counter += 1
    elif opcode == "PRINT":
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
        token_counter += 1
    elif opcode == "JUMP.EQ.0":
        label = parts[1]
        program.append(label)
        token_counter += 1
    elif opcode == "JUMP.GT.0":
        label = parts[1]
        program.append(label)
        token_counter += 1
    elif opcode == "JUMP.LT.0":
        label = parts[1]
        program.append(label)
        token_counter += 1
    elif opcode == "SET":
        variable_name = parts[1]
        variable_value = " ".join(parts[2:])
        if parts[2].startswith('"') and parts[2].endswith('"') or parts[2].startswith("'") and parts[2].endswith("'"):
            variable_value = parts[2][1:-1]
        else:
            try:
                variable_value = int(parts[2])
            except ValueError:
                print(f"Invalid value for SET command: {parts[2]}")
        if "[" in variable_name and "]" in variable_name:
            array_name, index = variable_name[:-1].split("[")
            index = int(index)
            if array_name not in variables:
                variables[array_name] = []
            while len(variables[array_name]) <= index:
                variables[array_name].append(0)
            variables[array_name][index] = variable_value
        else:
            variables[variable_name] = variable_value
    elif opcode == "CONCAT":
        concat_name = parts[1]
        variable_1 = parts[2]
        variable_2 = parts[3]
        if variable_1 in variables.keys() and variable_2 in variables.keys() and type(variables[variable_1]) == str and type(variables[variable_2]) == str:
            concat_string = variables[variable_1] + variables[variable_2]
            variables[concat_name] = concat_string
    elif opcode == "FOR":
        loops = {}
        variable_name = parts[1]
        start_value = int(parts[2])
        end_value = int(parts[3])
        variables[variable_name] = start_value
        loops[variable_name] = (token_counter, end_value)
    elif opcode == "ENDFOR":
        variable_name = parts[1]
        if variable_name in loops:
            start_position, end_value = loops[variable_name]
            if variables[variable_name] < end_value:
                variables[variable_name] += 1
                token_counter = start_position
            else:
                del loops[variable_name]
class Stack:

    def __init__(self, size):
        self.buf = [0 for _ in range(size)]
        self.sp    = -1

    def push(self, number):
        self.sp += 1
        self.buf[self.sp] = number
    
    def pop(self):
        number = self.buf[self.sp]
        self.sp -= 1
        return number
    
    def top(self):
        return self.buf[self.sp]
        

result = 0
choice = ""
pc = 0
stack = Stack(256)

while program[pc] != "HALT":
    opcode = program[pc]
    pc += 1

    if opcode == "PUSH":
        number = program[pc]
        pc += 1
        stack.push(number)
    elif opcode == "POP":
        stack.pop()
    elif opcode == "ADD":
        a = stack.pop()
        b = stack.pop()
        result = a + b
        stack.push(result)
    elif opcode == "SUB":
        a = stack.pop()
        b = stack.pop()
        result = a - b
        stack.push(result)
    elif opcode == "MUL":
        a = stack.pop()
        b = stack.pop()
        result = a * b
        stack.push(result)
    elif opcode == "DIV":
        a = stack.pop()
        b = stack.pop()
        result = a / b
        stack.push(result)
    if opcode == "RESULT":
        print("The result is: ", result)
    elif opcode == "PRINT":
        string_literal = program[pc]
        pc += 1
        if string_literal.startswith("$"):
            variable_name = string_literal[1:]
            if "[" in variable_name and "]" in variable_name:
                array_name, index = variable_name[:-1].split("[")
                index = int(index)
                if array_name in variables and index < len(variables[array_name]):
                    string_literal = str(variables[array_name][index])
                else:
                    string_literal = "Undefined"
            elif variable_name in variables:
                string_literal = str(variables[variable_name])
            else:
                string_literal = "Undefined"
        print(string_literal)
    elif opcode == "READ":
        number = int(input())
        stack.push(number)
    elif opcode == "CHOICE":
        while True:
            choice = str(input())
            if choice in ["ADD", "SUB", "MUL", "DIV"]:
                break
            else:
                print("Invalid choice. Please enter ADD, SUB, MUL, or DIV.")
        while choice != "EXIT":
            print("Enter the first number: ")
            stack.push(int(input()))
            print("Enter the second number: ")
            stack.push(int(input()))
            if choice == "ADD":
                a = stack.pop()
                b = stack.pop()
                result = b + a
                stack.push(result)
            elif choice == "SUB":
                a = stack.pop()
                b = stack.pop()
                result = b - a
                stack.push(result)
            elif choice == "MUL":
                a = stack.pop()
                b = stack.pop()
                result = b * a
                stack.push(result)
            elif choice == "DIV":
                a = stack.pop()
                b = stack.pop()
                result = b / a
                stack.push(result)
            print("The result is: ", result)
            print("Enter your choice (ADD, SUB, MUL, DIV, EXIT): ")
            choice = str(input())
            
    elif opcode == "WAIT":
        input("Press Enter to continue...")
