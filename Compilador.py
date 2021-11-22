import BasicCompiler


while True:
    text = input('$ ')
    result, error = BasicCompiler.run('<stdin>', text)

    if error: 
        print(error.as_string())
    else: 
        print(result)

