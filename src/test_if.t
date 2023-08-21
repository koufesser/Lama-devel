  $ lamac -ds "../../../../../src/test_if.lama"
  $ cp "test_if.sm" "../../../../../src/test_if.sm"
  $ ./Driver.exe -sml test_if.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll  -o output1.s
  $ clang -no-pie stdlib.o output.o  "../../../../../src/std.ll" || echo $?
  $ ./a.out < "../../../../../src/test_if.input"
