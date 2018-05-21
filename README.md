Unfinished Haskell bindings for the FoundationDB C client. Currently contains FFI bindings to all of the C API, plus a WIP `Transaction` monad for running transactions ([example](https://github.com/crclark/foundationdb-haskell/blob/master/tests/Properties.hs#L54)). So far, it's mostly untested.

### Generating options from `fdb.options`

FoundationDB provides [a specification](https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options) of available client options. We generate the `FoundationDB.Options` module from this file with the `generate-options` executable in this project.

To build the `generate-options` executable, pass the `with-generate-options` flag
when building. For example, when building with `stack`, the command is

```
stack build --flag foundationdb-haskell:with-generate-options
```

You can then invoke it on `fdb.options`.

```
stack exec generate-options -- --file ~/Downloads/fdb.options > src/FoundationDB/Options.hs
```

### To do

Useful hints [here](https://forums.foundationdb.org/t/creating-new-bindings/207).
