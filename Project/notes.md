# Miscellaneous notes

## Directory Server
1. Client1 sends open request
2. Directory server marks file open by Client1
3. Client1 makes some changes
4. Client1 closes file, sending updated file to directory server
5. Directory server updates its copy, marks file no longer open by Client1

