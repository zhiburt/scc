cargo run -- $1
status=$?
if [[ $status -ne 0 ]]; then
    exit 1
fi

name_exec=${1%.*}
gcc -m64 asm.s -o $name_exec
