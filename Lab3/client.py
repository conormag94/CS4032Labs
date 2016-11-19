import sys
import socket
import select

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

def prompt():
    sys.stdout.write('> ')
    sys.stdout.flush()

def test_join(chatroom_name, client_name):
    request = "JOIN_CHATROOM: {}\nCLIENT_IP: 0\nPORT: 0\nCLIENT_NAME: {}\n".format(chatroom_name, client_name)
    return request.encode()

def test_leave(chatroom_name, client_name):
    request = "LEAVE_CHATROOM: {}\nJOIN_ID: 1\nCLIENT_NAME: {}\n".format(chatroom_name, client_name)
    return request.encode()

def test_disconnect(client_name):
    request = "DISCONNECT: 0\nPORT: 0\nCLIENT_NAME: {}\n".format(client_name)
    return request.encode()

def test_chat(chatroom_name, client_name, message):
    request = "CHAT: {}\nJOIN_ID: 1\nCLIENT_NAME: {}\nMESSAGE: {}\n\n".format(
        chatroom_name, client_name, message
    )
    return request.encode()

"""
join room nickname
leave room nickname
disconnect nickname
chat room nickname message
"""
def main(arg=None):
    HOST = arg
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))

    prompt()
    while 1:
        socket_list = [sys.stdin, s]
        read_sockets, write_sockets, error_sockets = select.select(socket_list , [], [])

        for sock in read_sockets:
            # Incoming message from chat server
            if sock == s:
                data = sock.recv(4096)
                if not data :
                    print ('\nDisconnected from chat server')
                    sys.exit()
                else :
                    #print data
                    sys.stdout.write("\n" + data.decode())
                    prompt()
            # User entered a command
            else:
                inp = sys.stdin.readline()
                args = inp.split(" ")
                command = args[0]
                if command == "join":
                    chatroom = args[1]
                    nickname = args[2]
                    req = test_join(chatroom, nickname)
                    s.sendall(req)
                elif command == "leave":
                    chatroom = args[1]
                    nickname = args[2]
                    req = test_leave(args[1], args[2])
                    s.sendall(req)
                elif command == "disconnect":
                    nickname = args[1]
                    req = test_disconnect(args[1])
                    s.sendall(req)
                elif command == "chat":
                    chatroom = args[1]
                    nickname = args[2]
                    message = " ".join(args[3:])
                    req = test_chat(chatroom, nickname, message)
                    s.sendall(req)

if __name__ == '__main__':
    try:
        message = ' '.join(sys.argv[1:])
        main(message)
    except IndexError:
        main()
