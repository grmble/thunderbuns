# thunderbuns

Chat server "vaporware"

Current status: technological playground


## infrastructure - option 1

In use right now.

* sqlite as database, provides full text search
* single server only - no external queue

## infrastructure - option 2

* postgresql as database/queue
* minio for blob storage?
* bleve as for search

## infrastructure - option3

* scylladb/cassandra as database
* nsq as queue
* minio for blob storage
* bleve for search

This was used in attempt #1.

Lessons learned:

* Type safe DB API sorely needed, wanted to play with persistent 
  backend
* cassandra really wants query/command separation

### search options

* solr/es
* bleve - golang
* sqlite

### pubsub options

* etcd - but no gRPC from haskell will be a pain
* redis - fast, clustering support ?
* rabbitmq - not so fast
* jocko - kafka clone in golang
* nsq - distributed queue in golang
* postgresql - queue options

## notable haskell libraries

* dhall for configuration
* file-embed to embed files/directories
* jose-jwt for JWT
* warp/wai for http backend
* optparse-applicative to parse command line
* argon2/bcrypt etc via cryptonite
* stm-chan for TBChan


## inspiration

* gokit, especially metrics and logging: 
  https://godoc.org/github.com/go-kit/kit/metrics
  
## honorable mentions

* astaxie/bat: httpie in go - faster startup
* jq! must use when json is in play
