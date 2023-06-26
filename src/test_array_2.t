  $ cat > test_array.lama <<-EOF
  > fun f() {
  > var x = 1;
  > var m = 5;
  > var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun (z) {var y = x + z + m; printf("i am working %d", y)} ];
  > samples [4]
  > }
  > f()(2)
  > EOF
  $ cat test_array.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ lamac -ds test_array.lama
  $ ./Driver.exe -sml test_array.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll -o output1.s
  $ clang -no-pie stdlib.o output.o || echo $?
  $ ./a.out 
  $  gcc -no-pie output1.s stdlib.o -o a.out
  $ ./a.out 


