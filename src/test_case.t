  $ cat > test_array.lama <<-EOF
  > var n, x, y;
  > fun append (x, y) {
  >   case x of
  >     Nil         ->  y
  >   | Cons (h, t) ->  Cons (h, append (t, y))
  >   esac
  > }
  > fun printList (x) {
  >   case x of
  >     Nil         -> skip
  >   | Cons (h, t) -> write (h); printList (t)
  >   esac
  > }
  > n := read ();
  > x := Cons (1, Cons (2, Nil));
  > y := Cons (3, Cons (4, Nil));
  > printList (x);
  > printList (y);
  > printList (append (x, y));
  > printList (append (y, x))

  > EOF
  $ cat test_array.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ lamac -ds test_array.lama
  $ ./Driver.exe -sml test_array.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll -o output1.s
  $ clang -no-pie stdlib.o output.o "../../../../../src/std.ll" || echo $?
  $ ./a.out 