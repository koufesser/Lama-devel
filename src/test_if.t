  $ cat > test_array.lama <<-EOF
  > var n;
  > n := read ();
  > case 3 of
  > a -> write (a)
  > | _ -> write (0)
  > esac;
  > case 3 of
  > a -> write (a)
  > esac;
  > case 3 of
  > a@_ -> write (a)
  > esac;
  > case A (1, 2, 3) of
  > A             -> write (1)
  > | a@A (_, _, _) -> case a of
  >                   A (x, y, z) -> write (x); write (y); write (z)
  > esac
  > esac;
  > case A (1, 2, 3, 4, 5) of
  > A                 -> write (0)
  > | A (_)             -> write (1)
  > | A (_, _)          -> write (2)
  > | A (_, _, _)       -> write (3)
  > | A (_, _, _, _)    -> write (4)
  > | A (_, _, _, _, _) -> write (5)
  > esac;
  > write (A (1, 2, 3, 4, 5).length);
  > write (A (1, 2, 3, 4, 5)[0]);
  > write (A (1, 2, 3, 4, 5)[1]);
  > write (A (1, 2, 3, 4, 5)[2]);
  > write (A (1, 2, 3, 4, 5)[3]);
  > write (A (1, 2, 3, 4, 5)[4])
  > EOF
  $ lamac -ds test_array.lama
  $ cp "test_array.sm" "../../../../../src/test_array.sm"
  $ ./Driver.exe -sml test_array.sm -o curry1.o
  $ cp "output.ll" "../../../../../src/array1.ll"
  $ llc output.ll  -o output1.s
  $ clang -no-pie stdlib.o output.o "../../../../../src/std.ll" || echo $?
  $ ./a.out < "../../../../../src/test_read.input"
