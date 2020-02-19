cargo run -- $1
status=$?
if [[ $status -ne 0 ]]; then
    exit 1
fi

name_exec=${1%.*}
compiler="gcc"
if [[ CLANG_BACKEND -ne "" ]]
then
    compiler="clang"
fi

${compiler} -m64 asm.s -o $name_exec
