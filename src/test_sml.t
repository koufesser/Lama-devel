  $ cat > curry1.sm <<-EOF
  >  PUBLIC ("main")
  >  LABEL ("main")
  >  BEGIN ("main", 2, 0, [], [], [])
  >  SLABEL ("L1")
  >  LINE (1)
  >  CONST (0)
  >  CALL ("Lf", 1, false)
  >  SLABEL ("L2")
  >  END
  >  LABEL ("Lf")
  >  BEGIN ("Lf", 1, 0, [], ["x"], [{ blab="L4"; elab="L5"; names=[]; subs=[{ blab="L7"; elab="L8"; names=[]; subs=[{ blab="L15"; elab="L16"; names=[]; subs=[]; }; { blab="L11"; elab="L12"; names=[]; subs=[]; }]; }]; }])
  >  SLABEL ("L4")
  >  SLABEL ("L7")
  >  CONST (1)
  >  CJMP ("z", "L10")
  >  SLABEL ("L11")
  >  LD (Arg (0))
  >  CONST (2)
  >  BINOP ("+")
  >  SLABEL ("L12")
  >  JMP ("L6")
  >  LABEL ("L10")
  >  SLABEL ("L15")
  >  LD (Arg (0))
  >  CONST (3)
  >  BINOP ("+")
  >  SLABEL ("L16")
  >  JMP ("L6")
  >  SLABEL ("L8")
  >  LABEL ("L6")
  >  SLABEL ("L5")
  >  END
  > EOF
  $ cat curry1.sm
  fun f (x) { if 1 then x+2 else x+3 fi }
  f (0)
  $ ./Driver.exe -sml curry1.sm -o curry1.o
  src/Driver.ml 221
  Scope ([("f", (`Local, `Fun (["x"], Scope ([], If (Const (1), Scope ([], Binop ("+", Var ("x"), Const (2))), Scope ([], Binop ("+", Var ("x"), Const (3))))))))], Call (Var ("f"), [Const (0)]))
  fun f (x) 
  {
    if 1 then {
                x + 2 } else {
                 x + 3 } fi
  }
  f (0)
  
  [1]
  $ gcc stdlib.o output.o || echo $?
  $ ./a.out || echo $?
  2
