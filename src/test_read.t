  $ cat > test_read.lama <<-EOF
  > var x, y, z;
  > x := read (); 
  > y := read (); 
  > z := x * y * 3;
  > write (z)
  > EOF
  $ cat test_read.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ lamac -ds test_read.lama
  $ ./Driver.exe -sml test_read.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll -o output1.s
  $ clang -no-pie stdlib.o output.o  || echo $?
  $ ./a.out < "../../../../../src/test_read.input"
  $  gcc -no-pie output1.s stdlib.o  -o a.out
  $ ./a.out < "../../../../../src/test_read.input"
