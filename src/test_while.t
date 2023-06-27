  $ cat > test_while.lama <<-EOF
  > var s, n, p;
  > s := 0;
  > n := read ();
  > p := 2;
  > while n > 0 do (
  >   n := n / 2
  > )
  > od
  $ cat test_while.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ lamac -ds test_while.lama
  $ ./Driver.exe -sml test_while.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll -o output1.s
  $ clang -no-pie stdlib.o output.o  || echo $?
  $ ./a.out < "../../../../../src/test_while.input"