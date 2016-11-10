import sys
import socket

HOST = "10.62.0.176"
PORT = 8080

"""
Message format:

"HELO message here\n"

"""
def form_message(message=None):
    request = message.strip('\n') + '\n'
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

    req = form_message(arg)
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
