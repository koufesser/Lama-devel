  $ cat > test_array.lama <<-EOF
  > public fun split (n, k) {
  > var a = makeArray (k),
  >       m = n;
  > for var i = 0;, i < k, i := i + 1
  > do
  >   if i == k - 1
  >   then a[i] := m
  >   else 
  >     a [i] := random (m - k + i + 1) + 1;
  >     m := m - a [i]
  >   fi
  > od;
  > a
  > }
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
