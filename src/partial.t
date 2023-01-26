$ rm -fr *.o
$ cat > curry1.lama <<-EOF
> fun plus (a, b) { a+b }
> fun add1 () { plus(1) }
> add1(7)
> EOF
$ cat curry1.lama
$ ./Driver.exe -llvm curry1.lama -o curry1.o
$ gcc lama_stdlib.c -c -o stdlib.o
$ gcc stdlib.o output.o; echo $?
$ ./a.out
