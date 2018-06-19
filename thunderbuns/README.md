# thunderbuns

Chat server "vaporware"

It's really an excuse to play around with various technologies

## infrastructure

* scylladb as database
* etcd as waitable endpoint
* minio for blob storage
* solr or es as search engine .. or maybe something lighter?

### search options

* solr/es
* bleve - golang

### pubsub options

* etcd - but no gRPC from haskell will be a pain
* redis - fast, clustering support ?
* rabbitmq - not so fast
* jocko - kafka clone in golang

## haskell libraries

* dhall for configuration
* cql-io to access scylladb
* file-embed to embed files/directories
* jose-jwt for JWT
* servant for implementation of the web api.
  http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html
* optparse-applicative to parse command line
* ipfs ??? global shared filesystem ?
* purescript bindings via https://github.com/eskimor/servant-purescript
* argon2/bcrypt etc via cryptonite


## inspiration

* gokit, especially metrics and logging: 
  https://godoc.org/github.com/go-kit/kit/metrics
  
## honorable mentions

* astaxie/bat: httpie in go - faster startup
* jq! must use when json is in play
