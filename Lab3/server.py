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

# Action codes
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
        self.room_ref = len(chatrooms)
        self.next_client_id = 0
        self.name = name
        self.client_list = []
        self.add_client(server_socket, "Server")

    def add_client(self, client_sock, nickname):

        # If the client has not already joined, add them to the list of clients
        if not any(d['nickname'] == nickname for d in self.client_list):
            new_client = {
                "id": self.next_client_id,
                "nickname": nickname,
                "socket": client_sock
            }
            self.client_list.append(new_client)
            self.next_client_id += 1

            if new_client["id"] == 0:
                host = get_server_ip()
                port = get_port()
                print("{} [{}:{}] connected".format(new_client["nickname"], host, port))

            return new_client["id"]
        print(nickname, "already in chatroom")
        return -1

    def remove_client(self, clientsocket, client_name):
        client_location = self.find_client_by_name(client_name)
        if client_location >= 0:
            client = self.client_list[client_location]
            (host, port) = client["socket"].getpeername()
            nickname = client["nickname"]
            self.client_list.remove(client)
            self.next_client_id -= 1
            # self.broadcast_message("{} [{}:{}] disconnected".format(nickname, host, port))
            return client["id"]


    def broadcast_message(self, message):
        for client in self.client_list:
            if client["nickname"] == "Server":
                print("<" + self.name + ">: " + message)
            else:
                client["socket"].sendall((message + "\n").encode())

    def print_clients(self):
        print("'{}' members:".format(self.name))
        for client in self.client_list:
            socket_info = None
            if client["nickname"] == "Server":
                socket_info = client["socket"].getsockname()
            else:
                socket_info = client["socket"].getpeername()
            (ip, port) = socket_info
            print("{} [{}:{}]".format(client["nickname"], ip, port))

    def find_client_by_name(self, name):
        for i in range(len(self.client_list)):
            c = self.client_list[i]
            if c["nickname"] == name:
                return i
        return -1


def get_chatroom_index(chatroom_array, message):
    first_line = message.split('\n')[0]
    name = first_line.split(': ')[1]
    for i in range(len(chatroom_array)):
        if chatroom_array[i].name == name:
            return i
    return -1

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
def generate_response(message, desired_action, chatroom=None, join_id=None):

    response = "Default message\n"
    if desired_action == HELO:
        ip = ("IP:{}\n").format(get_server_ip())
        port = ("Port:{}\n").format(get_port())
        student_id = "StudentID:133323317\n"
        response = message + ip + port + student_id
    elif desired_action == KILL_SERVICE:
        response = "TERMINATING...\n"
    elif desired_action == JOIN_CHATROOM:
        response = "JOINED_CHATROOM: {}\nSERVER_IP: {}\n".format(chatroom.name, get_server_ip())
        response += "PORT: {}\nROOM_REF: {}\nJOIN_ID: {}\n".format(get_port(), chatroom.room_ref, join_id)
    elif desired_action == LEAVE_CHATROOM:
        response = "LEFT_CHATROOM: {}\nJOIN_ID:{}\n".format(chatroom.room_ref, join_id)
    elif desired_action == DISCONNECT:
        response = "You want to DISCONNECT\n"
    elif desired_action == MESSAGE_CHATROOM:
        lines = message.split("\n")
        response = lines[0] + "\n" + lines[2] + "\n" + lines[3] + "\n"
    return response

def handle_request(clientsocket, address, timeout):
    print("Handling")
    while True:
        request = clientsocket.recv(4096).decode()
        if not request:
            clientsocket.close()
            break
        desired_action = get_request_type(request)
        (host, port) = clientsocket.getpeername()
        
        if desired_action == HELO:
            response = generate_response(request, HELO)
            clientsocket.sendall(response.encode())
        elif desired_action == KILL_SERVICE:
            response = generate_response(request, KILL_SERVICE)
            clientsocket.sendall(response.encode())
            kill_server()
        # Actions where the client has specified a particular chat by name.
        if desired_action in [JOIN_CHATROOM, LEAVE_CHATROOM, MESSAGE_CHATROOM]:
            chatroom_index = get_chatroom_index(chatrooms, request)
            current_room = chatrooms[chatroom_index]

            if desired_action == JOIN_CHATROOM:
                client_name = request.split("\n")[3].split(": ")[1]
                join_id = current_room.add_client(clientsocket, client_name)
                if join_id == -1:
                    response = "Already connected to " + current_room.name + "\n"
                    clientsocket.sendall(response.encode())
                else:
                    response = generate_response(request, desired_action, current_room, join_id)
                    clientsocket.sendall(response.encode())
                    current_room.broadcast_message("CHAT: {}\nCLIENT_NAME: {}\nMESSAGE: {} has joined this chatroom.\n".format(current_room.room_ref, client_name, client_name))
            elif desired_action == LEAVE_CHATROOM:
                client_name = request.split("\n")[2].split(": ")[1]
                join_id = current_room.remove_client(clientsocket, client_name)
                response = generate_response(request, desired_action, current_room, join_id)
                print("*****\n" + response + "*****")
                clientsocket.sendall(response.encode())
                current_room.broadcast_message("CHAT: {}\nCLIENT_NAME: {}\nMESSAGE: {} has left this chatroom\n".format(current_room.room_ref, client_name, client_name))
                #TODO: Remove
                print("Breaking")
                break
            elif desired_action == MESSAGE_CHATROOM:
                response = generate_response(request, desired_action, current_room)
                current_room.broadcast_message(response)
        elif desired_action == DISCONNECT:
            client_name = request.split("\n")[2].split(": ")[1]
            for room in chatrooms:
                room.remove_client(clientsocket, client_name)
            clientsocket.sendall("***Disconnecting***\n".encode())
            (host, port) = clientsocket.getpeername()
            clientsocket.close()
            print("{} [{}:{}] disconnected from server".format(client_name, host, port))

    print("Returning...")


def main():
    set_server_ip(sys.argv[1])
    set_port(int(sys.argv[2]))

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
        print("Chat server started on port", port, '\nCtrl+C to quit\n')
    except Exception as e:
        if server:
            server.close()
        print(e)
        sys.exit(1)

    test_room = ChatRoom("room1", server)
    chatrooms.append(test_room)

    # Listen for incoming connections and pass off work to threads in pool
    while is_running():
        (clientsocket, address) = server.accept()
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
