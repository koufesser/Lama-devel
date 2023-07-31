  $ cat > test_array.lama <<-EOF
  > var n;
  > fun collect_ints_acc (v, tail) {
  > var i;
  > case v of
  >   a@#val ->  Cons (a, tail)
  > | #str   ->  tail
  > | _       ->
  >    for i := 0, i < v.length, i := i + 1 do
  >      tail := collect_ints_acc (v[i], tail)
  >    od;
  >     tail
  > esac
  > }
  > fun collect_ints (v) {
  >  collect_ints_acc (v, Nil)
  > }
  > collect_ints ([1, 2, 3])
  > EOF
  $ cat test_array.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ lamac -ds test_array.lama
  $ cp "test_array.sm" "../../../../../src/test_array.sm"
  $ ./Driver.exe -sml test_array.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll  -o output1.s
  $ clang -no-pie stdlib.o output.o "../../../../../src/std.ll" || echo $?
  $ ./a.out < "../../../../../src/test_read.input"
