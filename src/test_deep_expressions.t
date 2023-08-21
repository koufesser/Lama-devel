  $ cat > test.sh <<-EOF
  > #!/bin/bash
  > for i in {100..999}
  > do
  >   START="../../../../../regression/deep-expressions"
  >   if test -f "\${START}/generated00\${i}.lama"; then
  >   echo "test number: \$i"
  >   lamac -ds "\${START}/generated00\${i}.lama"
  >   ./Driver.exe -sml "generated00\${i}.sm" -o curry1.o
  >   llc output.ll -o output1.s
  >   clang -no-pie stdlib.o output.o "../../../../../src/std.ll"
  >   ./a.out < "\${START}/generated00\${i}.input" > "generated00\${i}.log" 
  >   diff  "generated00\${i}.log" "\${START}/orig/generated00\${i}.log"
  >   fi
  > done
  > EOF
  $ chmod +x test.sh
  $ ./test.sh
