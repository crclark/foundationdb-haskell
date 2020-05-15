#ifndef FDBC_WRAPPER
#define FDBC_WRAPPER

#ifndef FDB_API_VERSION
#error FDB_API_VERSION was not defined by the cabal file. Is CC-Options set?
#endif
#include "foundationdb/fdb_c.h"
#include "foundationdb/fdb_c_options.g.h"

// wasn't sure how to call a function-like macro in c2hs, so wrapped it here.
fdb_error_t select_api_version(int runtime_version);

#endif //FDBC_WRAPPER
