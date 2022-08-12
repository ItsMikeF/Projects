import requests

print("hello world")

response = requests.get('http://www.google.com')

print(response.status_code)
print(response.headers['content-type'])
print(response.headers['content-length'])