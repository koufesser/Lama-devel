  $ cat > curry1.lama <<-EOF
  > fun f () { var x := 100; fun apply (x, y) {y(x)} fun g(x)  }
  > f ()
  > EOF
  $ cat curry1.lama
  fun f () { var x, y, z, l, m; fun g () {x + 2} x:= 100; y := g; x:= 200; z:= g; l:= y(); m:=z() }
  f ()
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
