# Design

The idea is to have a small docker service that can store auths to other backends* and wrap secrets in a centralized fasion

Secret providers:
 - File in the local folder
 - BitWarden (using `bw` cli)
 - Git remote

## Glosary

`item` -> public name to access a resource
`resource` -> How to get some information