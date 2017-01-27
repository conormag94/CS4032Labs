# CS4032 Project: Distributed File System
Project for CS4032 - Distributed Systems. This is a Distributed File System based on the AFS model. This DFS was written in Haskell using the Servant framework. Each feature listed below is implemented in its own Servant project. 

These projects talk to each other by importing their APIs in their `stack.yaml` file. 

## Features implemented
 - File Server
 - Client
 - Directory Service
 
## FileObj
This is the datatype returned by a `GET` request to the file server and used as the request body in a `POST` request.

```Haskell
data FileObj = FileObj {
    name :: String
    content :: Text
}
```

## File Server
This is responsible for storing files and providing an API for clients to access those files. It has endpoints for downloading, uploading, modifying, deleting and listing files.

### File Server Usage
`stack setup`

`stack build`

`stack exec file-system-exe`

This will run on `localhost:8080`.

- `localhost:8080/files/<filename>`: Get file
- `localhost:8080/upload`: Post file
- `localhost:8080/delete/<filename>`: Delete file
- `localhost:8080/modify`: Modify file
- `localhost:8080/list`: List all files on server

## Client
User client for easier use of the File Server and Directory Service. Prompts the user for input and then sends their request to the appropriate service, displaying the result. It also stores the result on the user's file system if necessary.

### Client Usage
`stack setup`

`stack build`

`stack exec client-exe`

All commands are displayed when the client first starts. Enter `help` in the prompt to view them again.

## Directory Service
Responsible for finding which file server has a particular file and also for listing all the files in the distributed file system. At the moment there is only one file server running so the Directory Service outputs some dummy data for the purpose of getting all the components talking to each other properly.

### Directory Service Usage
`stack setup`

`stack build`

`stack exec directory-service-exe`

This will run on `localhost:8081`

  
