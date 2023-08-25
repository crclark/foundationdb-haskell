#!/bin/bash

# Based on the fdb-version value chosen by the matrix CI, set $FDB_VER_FLAG to
# the corresponding library version that cabal should build foundationdb-haskell
# to target. See also the flags in foundationdb-haskell.cabal, and the
# header fdbc_wrapper.h.

FDB_VER=${1}
echo "Got FDB_VER ${FDB_VER}"

if [[ ${FDB_VER} = "5.2.8" ]]
then
  export FDB_VER_FLAG='-f fdb-version-520'
elif [[ ${FDB_VER} = "6.0.18" ]]
then
  export FDB_VER_FLAG='-f fdb-version-600'
elif [[ ${FDB_VER} = "6.1.13" ]]
then
  export FDB_VER_FLAG='-f fdb-version-610'
elif [[ ${FDB_VER} = "6.2.20" ]]
then
  export FDB_VER_FLAG='-f fdb-version-620'
elif [[ ${FDB_VER} = "6.3.12" ]]
then
  export FDB_VER_FLAG='-f fdb-version-630'
elif [[ ${FDB_VER} = "7.0.0" ]]
then
  export FDB_VER_FLAG='-f fdb-version-700'
elif [[ ${FDB_VER} = "7.1.15" ]]
then
  export FDB_VER_FLAG='-f fdb-version-710'
elif [[ ${FDB_VER} = "7.2.7" ]]
then
  export FDB_VER_FLAG='-f fdb-version-720'
elif [[ ${FDB_VER} = "7.3.15" ]]
then
  # latest is the default, so no flag is specified
  export FDB_VER_FLAG=''
else
  echo "Error: unknown FDB_VER in ci/export-fdb-version-flag-cabal.sh"
  exit 1
fi

echo "Using flag ${FDB_VER_FLAG}"
