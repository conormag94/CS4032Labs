# Lab 2 - Multithreaded TCP server

**Python 3 is required**
### Usage
```
$ ./start.sh 8080
```
This will run server.py on port 8080

I used a slightly modified `client.py` from last lab to test locally. At the moment it is set to point towards my debian node but the ip can be changed to localhost if needed to test locally.
Once running on an opennebula node though, stephen's test form will reach it. This is probably easier.

To send "HELO your message here\n" to the server:
```
$ python3 client.py HELO your message here
```

