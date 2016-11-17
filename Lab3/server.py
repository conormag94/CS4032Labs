import os
import sys
import socket
import select
import threading

from concurrent.futures import ThreadPoolExecutor

# Constants
MAX_CONNECTIONS = 5
MAX_WORKERS = 5
TIMEOUT = 5

HELO = 0
KILL_SERVICE = 1
JOIN_CHATROOM = 2
LEAVE_CHATROOM = 3
DISCONNECT = 4
MESSAGE_CHATROOM = 5
DEFAULT = -1


# Global variables
server_ip = '10.62.0.176'
port = 8080
running = True
chatrooms = []

# Getters and setters for server_ip and port global variables
def get_server_ip():
    global server_ip
    return server_ip

def set_server_ip(new_ip):
    global server_ip
    server_ip = new_ip

def get_port():
    global port
    return port

def set_port(new_port):
    global port
    port = new_port

def is_running():
    global running
    return running

def kill_server():
    global running
    running = False


class ChatRoom(object):
    """
    Chatroom object to allow for easy creation of chatrooms
    """
    def __init__(self, name, server_socket):
        self.name = name
        self.client_list = [server_socket]

    def add_client(self, client_sock):
        print(self.client_list)
        if client_sock not in self.client_list:
            self.client_list.append(client_sock)
            print("Client added")
            return
        print("Client already in chatroom")

    def remove_client(self, client_sock, client_addr):
        client = (client_addr, client_sock)
        if client in self.client_list:
            self.client_list.remove(client)
            client.close()

    def broadcast_message(self, message):
        for client in self.client_list:
            client.sendall(message.encode())

    def print_clients(self):
        print("'{}' members:".format(self.name))
        for client in self.client_list:
            (host, port) = client.getsockname()
            print("<{}:{}>".format(host, port))


def get_request_type(message):
    request_type = DEFAULT
    if message[0:4] == "HELO":
        request_type = HELO
    elif message == "KILL_SERVICE\n":
        request_type = KILL_SERVICE
    elif message[0:13] == "JOIN_CHATROOM":
        request_type = JOIN_CHATROOM
    elif message[0:14] == "LEAVE_CHATROOM":
        request_type = LEAVE_CHATROOM
    elif message[0:10] == "DISCONNECT":
        request_type = DISCONNECT
    elif message[0:4] == "CHAT":
        request_type = MESSAGE_CHATROOM

    return request_type
# Parses the received message and generates the appropriate response
def generate_response(message, desired_action):

    response = "Default message\n"
    if desired_action == HELO:
        ip = ("IP:{}\n").format(get_server_ip())
        port = ("Port:{}\n").format(get_port())
        student_id = "StudentID:133323317\n"
        response = message + ip + port + student_id
    elif desired_action == KILL_SERVICE:
        response = "TERMINATING...\n"
        kill_server()
    elif desired_action == JOIN_CHATROOM:
        response = "You want to JOIN_CHATROOM\n"
    elif desired_action == LEAVE_CHATROOM:
        response = "You want to LEAVE_CHATROOM\n"
    elif desired_action == DISCONNECT:
        response = "You want to DISCONNECT\n"
    elif desired_action == MESSAGE_CHATROOM:
        response = "You want to CHAT\n"
    return response

def handle_request(clientsocket, address, timeout):
    request = clientsocket.recv(4096).decode()
    desired_action = get_request_type(request)
    response = generate_response(request, desired_action)
    print("[{},{}] - {}".format(address[0], address[1], request).strip('\n'))
    clientsocket.sendall(response.encode())
    clientsocket.close()

def main():
    set_port(int(sys.argv[1]))

    # Create new thread pool with maximum threads set to MAX_WORKERS
    thread_pool = ThreadPoolExecutor(max_workers=MAX_WORKERS)

    # Start the server
    try:
        server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind(('', get_port()))
        server.listen(MAX_CONNECTIONS)
        set_server_ip(server.getsockname()[0])
        set_port(server.getsockname()[1])
        print("Chat server started on port", port, '\nCtrl+C to quit')
    except Exception as e:
        if server:
            server.close()
        print(e)
        sys.exit(1)

    test_room = ChatRoom("Test Room", server)
    chatrooms.append(test_room)
    test_room.print_clients()

    # Listen for incoming connections and pass off work to threads in pool
    while is_running():
        (clientsocket, address) = server.accept()
        #test_room.add_client(clientsocket)
        #test_room.print_clients()
        thread = thread_pool.submit(handle_request, clientsocket, address, TIMEOUT)
    server.close()
    print("Terminating...")
    sys.exit(1)


if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print("Stopping server...")
        sys.exit(1)