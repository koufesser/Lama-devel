  $ cat > test_array.lama <<-EOF
  > var n, x, i;
  > x := "abcdefgh";
  > for i:=0, i<x.length, i:=i+1 do
  >  write (x[i])    
  > od
  > EOF
  $ cat test_array.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ lamac -ds test_array.lama
  $ ./Driver.exe -sml test_array.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll  -o output1.s
  $ clang -no-pie stdlib.o output.o "../../../../../src/std.ll" || echo $?
  $ ./a.out < "../../../../../src/test_read.input"
