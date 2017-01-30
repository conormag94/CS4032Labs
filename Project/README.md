# CS4032 Project: Distributed File System
Project for CS4032 - Distributed Systems. This is a Distributed File System based on the AFS model. This DFS was written in Haskell using the Servant framework. Each feature listed below is implemented in its own Servant project. 

These projects talk to each other by importing their APIs in their `stack.yaml` file. 

## Features implemented
 - File Server
 - Client
 - Caching (in Client)
 - Directory Service
 - Lock service
 
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
- `stack setup` if not already executed
- From `Project/file-system`, run `./run.sh`

This will run on `localhost:8080`.

- `localhost:8080/files/<filename>`: Get file
- `localhost:8080/upload`: Post file
- `localhost:8080/delete/<filename>`: Delete file
- `localhost:8080/modify`: Modify file
- `localhost:8080/list`: List all files on server

## Client
User client for easier use of the File Server and Directory Service. Prompts the user for input and then sends their request to the appropriate service, displaying the result. It also stores the result on the user's file system if necessary.

### Client Usage
- `stack setup` if not already executed
- Have the file system and directory service running
- From `Project/client`, run `./run.sh`

All commands are displayed when the client first starts. Enter `help` in the prompt to view them again.

## Caching
The caching is implemented in the Client component (`client/src/Client.hs`).

When the Client opens a file from the server, it is downloaded and stored on the client's file system. All reads and writes are done to this cached local copy. When the Client closes the file, this updated file is uploaded to the server and the client's cached copy is deleted.

## Directory Service
Responsible for finding which file server has a particular file and also for listing all the files in the distributed file system. At the moment there is only one file server running so the Directory Service outputs some dummy data for the purpose of getting all the components talking to each other properly.

### Directory Service Usage
- `stack setup` if not already executed
- Have file system running
- From `Project/directory-service`, run `./run.sh`

This will run on `localhost:8081`

## Lock Service
My approach is for the lock server to keep a database of `FileLock` data types. To lock a file, a `FileLock` for the file is stored in the mongoDB database. To unlock a file, its `FileLock` is deleted from the database. **So, a file is considered locked  if there is a FileLock for it in the server's database. A file is considered unlocked if there is no FileLock present**
```haskell
data FileLock = FileLock {
    fileName :: String
  , fileServer :: String
  , owner :: String
}
```

#### Locking a file:
- Client requests to lock a file by POSTing a `FileLock` to `localhost:8082/lockFile`
- Lock service checks its database to see if the file is already locked
- If it is locked: return an error message saying the file is already locked
- If it is unlocked: store a `FileLock` for the file in the database with the client as the owner

#### Unlocking a file:
- Client requests to unlock a file by sending DELETE with a `FileLock` to `localhost:8082/unlockFile`
- Lock service checks its database to see if the file is currently locked
- **If it is locked:** lock service checks to see if the client is the same client who originally locked the file
  - **If they originally locked the file:** delete the `FileLock` from the DB and send them a success message
  - **If the file was locked by somebody else:** return an error message stating as much
- **If it is unlocked:** return an error message saying the file is not locked

#### Integration with Client (in progress) + File Server (implemented)
The lock service has a `checkLock` endpoint which returns if a file is locked or not. This is used by the file server to make sure a Client isn't trying to access a file that is locked by someone else. The file server will deny access to a locked file if the client doesn't own the lock on the file. Since I have no authentication service set up, the owner is always set to the string "conor"

At the moment the lock service hasn't been integrated with the client so it must be used with cURL, attaching a JSON body in the same format as a `FileLock`.

### Lock service usage
- `stack setup` if not already executed
- Have mongoDB running on default port with a database called "locks"
- From `Project/directory-service`, run `./run.sh`
