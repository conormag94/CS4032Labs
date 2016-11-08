import sys
import socket

HOST = "localhost"
PORT = 8000

"""
Get request format:

GET /echo.php?message=test HTTP/1.1 [CRLF]
Host: hostname:portnumber [CRLF]
[CRLF]
"""
def form_get_request(message=None):
    parameter = ''
    if message:
        parameter = '?message=' + message.replace(' ', '%20')
    request_line = 'GET /echo.php' + parameter + ' HTTP/1.1\r\n'

    host_line = 'Host: {}:{}\r\n'.format(HOST, PORT)
    request = request_line + host_line + '\r\n'
    return request.encode()

def receive_data(s):
    while 1:
        data = s.recv(4096)
        if (len(data) > 0):
            print(data.decode())
        else:
            return data.decode()

def main(arg=None):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))

    req = form_get_request(arg)
    s.send(req)

    data = receive_data(s)
    print(data)

    s.close()

if __name__ == '__main__':
    try:
        message = ' '.join(sys.argv[1:])
        main(message)
    except IndexError:
        main()
