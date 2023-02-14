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
  fun fibk (n) {
    fun f1 () { n }
    fun f2 () { n }
    var x = fun () { f1() + f2 () + n };
    fun () { f1() + f2 () + x () }
  }
  $ ./Driver.exe -llvm curry1.lama -o curry1.o
  src/Driver.ml 221
  Scope ([("fibk", (`Local, `Fun (["n"], Scope ([("f1", (`Local, `Fun ([], Scope ([], Var ("n"))))); ("f2", (`Local, `Fun ([], Scope ([], Var ("n"))))); ("x", (`Local, `Variable (Some (Lambda ([], Scope ([], Binop ("+", Binop ("+", Call (Var ("f1"), []), Call (Var ("f2"), [])), Var ("n"))))))))], Lambda ([], Scope ([], Binop ("+", Binop ("+", Call (Var ("f1"), []), Call (Var ("f2"), [])), Call (Var ("x"), []))))))))], Seq (Skip, Const (0)))
  free vars inside `Scope ([], Binop ("+", Binop ("+", Call (Var ("f1"), []), Call (Var ("f2"), [])), Var ("n")))` are:
  {set| f1, f2, n, |set}
  free vars inside `Scope ([], Binop ("+", Binop ("+", Call (Var ("f1"), []), Call (Var ("f2"), [])), Call (Var ("x"), [])))` are:
  {set| f1, f2, x, |set}
  public fun fresh_1 (f1,f2,n)
  {
    f1 () + f2 () + n
  }
  public fun fresh_2 (f1,f2,x)
  {
    f1 () + f2 () + x ()
  }
  fun fibk (n)
  {
    fun f1 ()
    {
      n
    }
    fun f2 ()
    {
      n
    }
    local x = fresh_1 (f1, f2, n);
    fresh_2 (f1, f2, x)
  }
  skip;
  0

  [2]
$ cat > curry1.lama <<-EOF
> fun even (n) { if (n>0) then odd (n-1) else if n=0 then true else false fi fi }
> fun odd  (n) { if (n>0) then even (n-1) else if n=0 then false else true fi fi }
> EOF
$ cat curry1.lama
$ ./Driver.exe -llvm curry1.lama -o curry1.o
