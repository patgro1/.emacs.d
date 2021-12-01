
readonly GCC_DIR="$(realpath $(brew --prefix libgccjit))"
[[ -d $GCC_DIR ]] ||  { echo "${GCC_DIR} not found"; exit 1; }

readonly SED_DIR="$(realpath $(brew --prefix gnu-sed))"
[[ -d $SED_DIR ]] ||  { echo "${SED_DIR} not found"; exit 1; }

readonly GCC_INCLUDE_DIR=${GCC_DIR}/include
[[ -d $GCC_INCLUDE_DIR ]] ||  { echo "${GCC_INCLUDE_DIR} not found"; exit 1; }

readonly GCC_LIB_DIR=${GCC_DIR}/lib/gcc/10
[[ -d $GCC_LIB_DIR ]] ||  { echo "${GCC_LIB_DIR} not found"; exit 1; }

export PATH="${SED_DIR}/libexec/gnubin:${PATH}"
export CFLAGS="-O2 -I${GCC_INCLUDE_DIR}"
export LDFLAGS="-L${GCC_LIB_DIR} -I${GCC_INCLUDE_DIR}"
export LD_LIBRARY_PATH="${GCC_LIB_DIR}"
export DYLD_FALLBACK_LIBRARY_PATH="${GCC_LIB_DIR}"

echo "Environment"
echo "-----------"
echo ATH: $PATH
echo CFLAGS: $CFLAGS
echo LDFLAGS: $LDFLAGS
echo DYLD_FALLBACK_LIBRARY_PATH: $DYLD_FALLBACK_LIBRARY_PATH
echo "-----------"P
