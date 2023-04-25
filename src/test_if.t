  $ cat > curry1.lama <<-EOF
  > fun f (x) { var y; if 1 then x+2 else x+3 fi }
  > f (0)
  > EOF
  $ cat curry1.lama
  fun f (x) { if 1 then x+2 else x+3 fi }
  f (0)
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
  