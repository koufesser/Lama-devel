  $ cat > test_read.lama <<-EOF
  > var x;
  > x := "abcdefgh";
  > x[0] := x[0]+2
  > EOF
  $ lamac -ds test_read.lama
  $ ./Driver.exe -sml test_read.sm -o output.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll -o output1.s
  $ clang -no-pie stdlib.o output.o  || echo $?
  $ ./a.out < "../../../../../src/test_read.input"
