[![Build Status](https://travis-ci.org/crclark/foundationdb-haskell.svg?branch=master)](https://travis-ci.org/crclark/foundationdb-haskell)

Unfinished Haskell bindings for the FoundationDB C client. Under active development. Currently contains FFI bindings to all of the C API, a `Transaction` monad for running transactions ([example](https://github.com/crclark/foundationdb-haskell/blob/1f8d0ba2c4985d2fe3d8e6fcbc852c01050af9bb/tests/Properties.hs#L48)), and implementations of the standard tuple, subspace and directory layers. Tests are still sparser than I'd like, and the API is still changing.

### Supported FoundationDB versions

Currently supports 5.2.x and 6.0.x.


### Generating options from `fdb.options`

FoundationDB provides [a specification](https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options) of available client options. We generate the `FoundationDB.Options` module from this file with the `generate-options` executable in this project.

To build the `generate-options` executable, pass the `with-generate-options` flag
when building. For example, when building with `stack`, the command is

```
stack build --flag foundationdb-haskell:with-generate-options
```

You can then invoke it on `fdb.options`.

```
stack exec generate-options -- --file /usr/include/foundationdb/fdb.options > src/FoundationDB/Options.hs
```

### Running tests

The tests read, write, and delete keys from a database. The database to use in the tests is specified by the `FDB_HASKELL_TEST_CLUSTER_FILE` environment variable. For example, using `stack`, one might invoke the tests like this

```
FDB_HASKELL_TEST_CLUSTER_FILE=/etc/foundationdb/fdb.cluster stack test
```


### To do

Useful hints [here](https://forums.foundationdb.org/t/creating-new-bindings/207).
