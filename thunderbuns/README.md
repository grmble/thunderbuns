# thunderbuns

Chat server "vaporware"

Current status: technological playground

## infrastructure

* postgresql as database/queue
* minio for blob storage?
* solr or es as search engine .. or maybe something lighter?

### search options

* solr/es
* bleve - golang

### pubsub options

* etcd - but no gRPC from haskell will be a pain
* redis - fast, clustering support ?
* rabbitmq - not so fast
* jocko - kafka clone in golang
* nsq - distributed queue in golang
* postgresql - queue options

## haskell libraries

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
