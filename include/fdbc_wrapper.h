#ifndef FDBC_WRAPPER
#define FDBC_WRAPPER

#ifndef FDB_API_VERSION
#error FDB_API_VERSION was not defined by the cabal file. Is CC-Options set?
#endif
#include "foundationdb/fdb_c.h"
#include "foundationdb/fdb_c_options.g.h"

// wasn't sure how to call a function-like macro in c2hs, so wrapped it here.
fdb_error_t select_api_version(int runtime_version);

#if FDB_API_VERSION >= 710

// GHC can't handle nested structs, so this helper function
// "flattens" the input FDBMappedKeyValue struct.
void get_mapped_range_result(FDBMappedKeyValue *mapped_key_value,

                             // parent (secondary index) k/v
                             uint8_t **out_parent_key,
                             int *out_parent_key_length,
                             uint8_t **out_parent_value,
                             int *out_parent_value_length,

                             // begin key selector
                             uint8_t **out_begin_key_sel_key,
                             int *out_begin_key_sel_key_length,
                             fdb_bool_t *out_begin_key_sel_or_equal,
                             int *out_begin_key_sel_offset,

                             // end key selector
                             uint8_t **out_end_key_sel_key,
                             int *out_end_key_sel_key_length,
                             fdb_bool_t *out_end_key_sel_or_equal,
                             int *out_end_key_sel_offset,

                             // the k/vs from the primary index
                             FDBKeyValue **out_data,
                             // m_size in fdb_c.h
                             int *out_data_length);

int get_mapped_key_value_size();

#endif

#endif // FDBC_WRAPPER
