  $ cat > test_array.lama <<-EOF
  > var samples = [ "string 1", 2, "string2"];
  > printf ("%s\n", samples[0]);
  > printf ("%d\n", samples[1]);
  > printf ("%s\n", samples[2])
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
