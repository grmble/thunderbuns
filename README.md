# thunderbuns

## infrastructure

* scylladb as database
* etcd as waitable endpoint
* minio for blob storage
* solr as search engine .. or maybe something lighter?

## haskell libraries

* dhall for configuration
* cql-io to access scylladb
* file-embed to embed files/directories
* servant for implementation of the web api.
  http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html
* cmdargs to parse command line
* ipfs ??? global shared filesystem ?
* purescript bindings via https://github.com/eskimor/servant-purescript


## inspiration

* gokit, especially metrics and logging: 
  https://godoc.org/github.com/go-kit/kit/metrics
