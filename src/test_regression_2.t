  $ cat > test.sh <<-EOF
  > #!/bin/bash
  > for i in {10..99}
  > do
  >   START="../../../../../regression"
  >   if test -f "\${START}/test0\${i}.lama"; then
  >   echo "test number: \$i"
  >   lamac -ds "\${START}/test0\${i}.lama"
  >   ./Driver.exe -sml "test0\${i}.sm" -o curry1.o
  >   cp "output.ll" "../../../../../src/array1.ll"
  >   llc output.ll -o output1.s
  >   clang -Wno-everything -no-pie stdlib.o output.o "../../../../../runtime/runtime.c"
  >   ./a.out < "\${START}/test0\${i}.input" > "test0\${i}.log" 
  >   diff  "test0\${i}.log" "\${START}/orig/test0\${i}.log"
  >   fi
  > done
  > EOF
  $ chmod +x test.sh
  $ ./test.sh
