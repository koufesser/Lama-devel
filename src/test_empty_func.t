  $ cat > curry1.lama <<-EOF
  > fun f() {var x, y; y := 3; x:= (2 + y); x}
  > f()
  > EOF
  $ cat curry1.lama
  fun f() {2}
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
