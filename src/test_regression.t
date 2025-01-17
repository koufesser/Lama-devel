  $ cat > test.sh <<-EOF
  > #!/bin/bash
  > for i in {1..9}
  > do
  >   echo "test number: \$i"
  >   START="../../../../../regression"
  >   lamac -ds "\${START}/test00\${i}.lama"
  >   ./Driver.exe -sml "test00\${i}.sm" -o curry1.o
  >   cp "output.ll" "../../../../../src/array1.ll"
  >   llc output.ll -o output1.s
  >   clang -no-pie stdlib.o output.o
  >   ./a.out < "\${START}/test00\${i}.input" > "test00\${i}.log" 
  >   diff  "test00\${i}.log" "\${START}/orig/test00\${i}.log"
  > done
  > EOF
  $ chmod +x test.sh
  $ ./test.sh
