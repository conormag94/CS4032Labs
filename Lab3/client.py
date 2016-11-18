import sys
import socket

HOST = "localhost"
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

def test_join(chatroom_name, client_name):
    request = "JOIN_CHATROOM: {}\nCLIENT_IP: 0\nPORT: 0\nCLIENT_NAME: {}\n".format(chatroom_name, client_name)
    return request.encode()

def test_leave(chatroom_name, client_name):
    request = "LEAVE_CHATROOM: {}\nJOIN_ID: 1\nCLIENT_NAME: {}\n".format(chatroom_name, client_name)
    return request.encode()

def main(arg=None):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))

    #req = form_message(arg)
    if sys.argv[-1] == "join":
        req = test_join("Test Room", "Conor")
        s.send(req)
    if sys.argv[-1] == "leave":
        req = test_leave("Test Room", "Conor")
        s.send(req)

    data = s.recv(4096).decode()
    print(data)

    data = s.recv(4096).decode()
    print(data)

    # data = receive_data(s)
    # print(data)


    s.close()

if __name__ == '__main__':
    try:
        message = ' '.join(sys.argv[1:])
        main(message)
    except IndexError:
        main()
