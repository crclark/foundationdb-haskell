[![Build Status](https://travis-ci.org/crclark/foundationdb-haskell.svg?branch=master)](https://travis-ci.org/crclark/foundationdb-haskell)

[API Docs](https://crclark.github.io/foundationdb-haskell/)

Haskell bindings for the FoundationDB C client. Currently contains FFI bindings to all of the C API, a `Transaction` monad for running transactions ([example](https://github.com/crclark/foundationdb-haskell/blob/1f8d0ba2c4985d2fe3d8e6fcbc852c01050af9bb/tests/Properties.hs#L48)), and implementations of the standard tuple, subspace and directory layers.

I am not using this in a production-like context, but I have exercised the code rather heavily (heavy transaction rates for days at a time) in both local and cloud environments. The major missing component is directory partitions, but I haven't needed them yet. Generally speaking, the directory layer is the least exercised part of the library.

### Supported FoundationDB versions

Currently supports 5.2.x, 6.0.x, 6.1.x, and 6.2.x.


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
