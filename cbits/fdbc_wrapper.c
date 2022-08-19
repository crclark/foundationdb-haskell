#include <string.h>
#include <stdlib.h>
#include <fdbc_wrapper.h>

fdb_error_t select_api_version(int runtime_version)
{
  fdb_select_api_version(runtime_version);
}

#if FDB_API_VERSION >= 710

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
                             int *out_data_length)
{

  // NOTE: memcpy is used because the original pointers in the structs are const
  *out_parent_key_length = mapped_key_value->key.key_length;
  *out_parent_key = malloc(*out_parent_key_length * sizeof(uint8_t));
  memcpy(*out_parent_key, mapped_key_value->key.key, *out_parent_key_length);

  *out_parent_value_length = mapped_key_value->value.key_length;
  *out_parent_value = malloc(*out_parent_value_length * sizeof(uint8_t));
  memcpy(*out_parent_value, mapped_key_value->value.key, *out_parent_value_length);

  *out_begin_key_sel_key_length = mapped_key_value->getRange.begin.key.key_length;
  *out_begin_key_sel_key = malloc(*out_begin_key_sel_key_length * sizeof(uint8_t));
  memcpy(*out_begin_key_sel_key, mapped_key_value->getRange.begin.key.key, *out_begin_key_sel_key_length);

  *out_begin_key_sel_or_equal = mapped_key_value->getRange.begin.orEqual;

  *out_begin_key_sel_offset = mapped_key_value->getRange.begin.offset;

  *out_end_key_sel_key_length = mapped_key_value->getRange.end.key.key_length;
  *out_end_key_sel_key = malloc(*out_end_key_sel_key_length * sizeof(uint8_t));
  memcpy(*out_end_key_sel_key, mapped_key_value->getRange.end.key.key, *out_end_key_sel_key_length);

  *out_end_key_sel_or_equal = mapped_key_value->getRange.end.orEqual;
  *out_end_key_sel_offset = mapped_key_value->getRange.end.offset;

  *out_data = mapped_key_value->getRange.data;
  *out_data_length = mapped_key_value->getRange.m_size;
}

int get_mapped_key_value_size()
{
  return sizeof(FDBMappedKeyValue);
}

#endif
