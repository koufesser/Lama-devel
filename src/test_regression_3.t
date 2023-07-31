  $ cat > test.sh <<-EOF
  > #!/bin/bash
  > for i in {100..110}
  > do
  >   START="../../../../../regression"
  >   if test -f "\${START}/test\${i}.lama"; then
  >   echo "test number: \$i"
  >   lamac -ds "\${START}/test\${i}.lama"
  >   ./Driver.exe -sml "test\${i}.sm" -o curry1.o
  >   cp "output.ll" "../../../../../src/array1.ll"
  >   llc output.ll -o output1.s
  >   clang -no-pie stdlib.o output.o "../../../../../src/std.ll"
  >   ./a.out < "\${START}/test\${i}.input" > "test\${i}.log" 
  >   diff  "test\${i}.log" "\${START}/orig/test\${i}.log"
  >   fi
  > done
  > EOF
  $ chmod +x test.sh
  $ ./test.sh
