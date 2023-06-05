  $ cat > curry1.lama <<-EOF
  > var samples = [ "string 1", "string 2", 5];
  > printf("%s", samples[1]);
  > printf("%d", samples[2])
  > EOF
  $ cat curry1.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
  $ clang -no-pie stdlib.o output.o || echo $?
  $ ./a.out || echo $?