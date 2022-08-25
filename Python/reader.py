#create function to read file

def read_file(filename):
    try:
        with open(filename) as f_obj:
            contents = f_obj.read()
    except FileNotFoundError:
        pass
    else:
        print(contents)

read_file('data.csv')