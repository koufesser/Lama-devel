  $ ./Driver.exe -llvm llvm1.lama -o llvm1.o
  $ file output.o
  $ gcc output.o
  $ ./a.out || echo "FUCK" $?
  $ echo $?
