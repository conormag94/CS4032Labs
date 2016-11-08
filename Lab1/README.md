# Echo Server

## CS4032 Distributed Systems Lab 1

- Client sends GET request to echo server
- Echo server converts request to uppercase and returns it to client
- Client prints out server's response


#### Usage

Run php server locally in the directory containing `echo.php`:
```
$ php -S 0.0.0.0:8000 -t .
```  

Run python script with no message:

```
$ python3 client.py
```

Run python script with a message:
```
$ python3 client.py hello world
```

Note: No need for quotes when running with a message as the program joins all arguments into a single message.
