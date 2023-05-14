  $ cat > curry1.lama <<-EOF
  > fun f() {var y = 3; var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), 5]; samples[4]}
  > f()
  > EOF
  $ cat curry1.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
