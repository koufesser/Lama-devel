$ cat > curry1.lama <<-EOF
> fun plus (a, b) { fun (x) { var x = fun (y) { 42 } ; x (1) + 1 } }
> EOF
$ cat curry1.lama
$ ./Driver.exe -llvm curry1.lama -o curry1.o

> fun fib (n) { if n<2 then 1 else fib (n-1) + fib (n-2) fi }

$ cat > curry1.lama <<-EOF
> fun fibk (n,k) {
>   if n<2 then 1 else
>     fibk (n-1, fun (l) { fibk(n-2, fun (r) {
>       l+r
>     }) })
>   fi
> }
> EOF
  $ cat > curry1.lama <<-EOF
  > fun fibk (n) {
  >   fun f1 () { n }
  >   fun f2 () { n }
  >   var x = fun () { f1() + f2 () + n };
  >   fun () { f1() + f2 () + x () }
  > }
  > EOF
  $ cat curry1.lama
  $ ./Driver.exe -llvm curry1.lama -o curry1.o
$ cat > curry1.lama <<-EOF
> fun even (n) { if (n>0) then odd (n-1) else if n=0 then true else false fi fi }
> fun odd  (n) { if (n>0) then even (n-1) else if n=0 then false else true fi fi }
> EOF
$ cat curry1.lama
$ ./Driver.exe -llvm curry1.lama -o curry1.o