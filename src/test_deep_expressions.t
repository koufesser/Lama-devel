  $ cat > test.sh <<-EOF
  > #!/bin/bash
  > for i in {10..99}
  > do
  >   START="../../../../../regression/deep-expressions"
  >   if test -f "\${START}/generated000\${i}.lama"; then
  >   echo "test number: \$i"
  >   lamac -ds "\${START}/generated000\${i}.lama"
  >   ./Driver.exe -sml "generated000\${i}.sm" -o curry1.o
  >   llc output.ll -o output1.s
  >   clang -no-pie stdlib.o output.o "../../../../../src/std.ll"
  >   ./a.out < "\${START}/generated000\${i}.input" > "generated000\${i}.log" 
  >   diff  "generated000\${i}.log" "\${START}/orig/generated000\${i}.log"
  >   fi
  > done
  > EOF
  $ chmod +x test.sh
  $ ./test.sh
