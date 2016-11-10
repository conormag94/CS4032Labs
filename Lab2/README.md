# Lab 2 - Multithreaded TCP server

**Python 3 is required**
### Usage
- **Start the server**
```
$ ./start.sh 8080
```
This will run server.py on 0.0.0.0 port 8080

- **Test locally**
```
$ python3 client.py HELO your message here
```
This will send "HELO your message here\n" to the server. I used a slightly modified `client.py` from last lab for local testing. It is pointed towards localhost:8080 so make sure the server is running on 0.0.0.0:8080.

  **OR**

- **Test using stephen's form**

  Once the server is running on an opennebula node, stephen's test form will reach it. This is probably easier.
  
