  $ cat > curry1.lama <<-EOF
  > var y = 3;
  > var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {var z = y;} ];
  > var sample1 = samples [0] [0];
  > var sample2 = samples [1];
  > var sample3 = samples [2];
  > var sample4 = samples [4];
  > var l = sample4();
  > EOF
  $ cat curry1.lama
  var samples = [ {"a", "b", "c"}, "string", [], Fruit ("apple"), fun () {skip} ];
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
