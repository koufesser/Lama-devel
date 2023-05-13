  $ cat > curry1.sm <<-EOF
  > LABEL ("main")
  > BEGIN ("main", 2, 0, [], [], [])
  > SLABEL ("L1")
  > LINE (1)
  > STRING ("if, case, for, while etc. are all expressions.\\n")
  > CALL ("Lprintf", 1, false)
  > SLABEL ("L2")
  > END
  > EOF
  $ cat curry1.sm
  $ ./Driver.exe -sml curry1.sm -o curry1.o
  $ gcc -fPIE stdlib.o output.o || echo $?
  $ ./a.out || echo $?
  2