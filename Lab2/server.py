import sys
import socket
import threading
import concurrent.futures


MAX_CONNECTIONS = 5

port = 8000

def generate_response(message, address):
    response = "Default message"

    if message[0:4] == "HELO":
        ip = ("IP: {}\n").format(address[0])
        port = ("Port: {}\n").format(address[1])
        student_id = "StudentID: 133323317\n"
        response = message + ip + port + student_id

    elif message == "KILL_SERVICE\n":
        response = "TERMINATING..."

    return response

def handle_request(clientsocket, address, timeout):
    request = clientsocket.recv(4096).decode()
    response = generate_response(request, address)

    clientsocket.send(response.encode())
    clientsocket.close()

def main():
    port = int(sys.argv[1])
    # Start the server
    try:
        server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind(('', port))
        server.listen(MAX_CONNECTIONS)
        print("Listening on port", port)
        print("Ctrl+C to quit\n")
    except Exception as e:
        if server:
            server.close()
        print(e)
        sys.exit(1)

    while True:
        (clientsocket, address) = server.accept()
        threading.Thread(target=handle_request, args=(clientsocket, address, 60)).start()
    server.close()


if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print("Stopping server...")
        sys.exit(1)
