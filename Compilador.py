import os
import BasicCompiler
import sys
import time
import os

'''
welcome = "WELCOME TO LUIGUI LANGUAJE SUPER PRO VERSION"

for char in welcome:
    sys.stdout.write(char)
    sys.stdout.flush()
    time.sleep(0.1)
print("\n")

'''

while True:
    text = input('$ ')
    result, error = BasicCompiler.run('<stdin>', text)

    if error: 
        print(error.as_string())
    elif result: 
        print(result)

