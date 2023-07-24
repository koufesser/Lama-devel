  $ cat > test.sh <<-EOF
  > #!/bin/bash
  > for i in {40..42}
  > do
  >   echo "test number: \$i"
  >   START="../../../../../regression"
  >   lamac -ds "\${START}/test0\${i}.lama"
  >   ./Driver.exe -sml "test0\${i}.sm" -o curry1.o
  >   cp "output.ll" "../../../../../src/array1.ll"
  >   llc output.ll -o output1.s
  >   clang -no-pie stdlib.o output.o "../../../../../src/std.ll"
  >   ./a.out < "\${START}/test0\${i}.input" > "test0\${i}.log" 
  >   diff  "test0\${i}.log" "\${START}/orig/test0\${i}.log"
  > done
  > EOF
  $ chmod +x test.sh
  $ ./test.sh
