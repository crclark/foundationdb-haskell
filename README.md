Unfinished Haskell bindings for the FoundationDB C client. Currently consists of one untested module which is 1:1 equivalent to the [functions exported by the C API](https://apple.github.io/foundationdb/api-c.html). The plan is to build an idiomatic Haskell library on top of it, which will probably live in another repository.

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
