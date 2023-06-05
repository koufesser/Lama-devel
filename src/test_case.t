  $ cat > test_array.lama <<-EOF
  > case E of
  > A -> printf ("Nope. This is A\n")
  > | AB -> printf ("Nope. This is AB\n")
  > | B (x) -> printf ("Nope. This is B(x)\n")
  > | E -> printf ("Yes, this is E.\n") 
  > esac
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