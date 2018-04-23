#include <fdbc_wrapper.h>

fdb_error_t select_api_version(int runtime_version){
  fdb_select_api_version(runtime_version);
}
