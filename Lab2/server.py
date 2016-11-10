import os
import sys
import socket
import threading

from concurrent.futures import ThreadPoolExecutor

# Constants
MAX_CONNECTIONS = 5
MAX_WORKERS = 5
TIMEOUT = 5

# Global variables
server_ip = '10.62.0.176'
port = 8080
running = True

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

# Parses the received message and generates the appropriate response
def generate_response(message, address):
    response = "Default message"
    if message[0:4] == "HELO":
        ip = ("IP:{}\n").format(get_server_ip())
        port = ("Port:{}\n").format(get_port())
        student_id = "StudentID:133323317\n"
        response = message + ip + port + student_id
    elif message == "KILL_SERVICE\n":
        response = "TERMINATING...\n"
        kill_server()
    return response

def handle_request(clientsocket, address, timeout):
    request = clientsocket.recv(4096).decode()
    response = generate_response(request, address)
    print("[{},{}] - {}".format(address[0], address[1], request).strip('\n'))
    clientsocket.send(response.encode())
    clientsocket.close()

def main():
    set_port(int(sys.argv[1]))

    # Create new thread pool with maximum threads set to MAX_WORKERS
    thread_pool = ThreadPoolExecutor(max_workers=MAX_WORKERS)

    # Start the server
    try:
        server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind((get_server_ip(), get_port()))
        server.listen(MAX_CONNECTIONS)
        print("Listening on port", port, '\nCtrl+C to quit')
    except Exception as e:
        if server:
            server.close()
        print(e)
        sys.exit(1)

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
