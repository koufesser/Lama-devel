  $ cat > test_array.lama <<-EOF
  > var i, s;
  > i := 0;
  > s := 0; 
  > while i < 100
  > do
  >   s := s + i;
  >   i := i + 1
  > od;
  > write (s)
  > EOF
  $ lamac -ds test_array.lama
  $ ./Driver.exe -sml test_array.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll -o output1.s
  $ clang -no-pie stdlib.o output.o "../../../../../src/std.ll" || echo $?
  $ ./a.out