  $ #!/bin/bash
  $ for i in {1..5}
  $ do
  $   echo "Welcome $i times"
  $   START="../../../../../regression"
  $   lamac -ds "${START}/test00${i}.lama"
  $   ./Driver.exe -sml "test00${i}.sm" -o curry1.o
  $   cp "output.ll" "../../../../../src/array1.ll"
  $   llc output.ll -o output1.s
  $   clang -no-pie stdlib.o output.o || echo $?
  $   ./a.out < "${START}/test00${i}.input"
  $ done
 
