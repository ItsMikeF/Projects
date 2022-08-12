msg = "hello world"
print(msg)

for i in range(10):
    print(i)

import json

items = json.loads('[{"name": "John", "age": 30}, {"name": "Mary", "age": 28}]')

for item in items:
    print(item['name'])
    print(item['age'])