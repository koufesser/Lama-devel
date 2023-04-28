  $ cat > curry1.lama <<-EOF
  > fun f (x) { var y; if 1 then var z = 4; x+2+z else x+3 fi }
  > f (0)
  > EOF
  $ cat curry1.lama
  fun f (x) { var y; if 1 then var z = 4; x+2+z else x+3 fi }
  f (0)
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o
  