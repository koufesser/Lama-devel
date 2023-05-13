  $ cat > curry1.lama <<-EOF
  > var i, s = 0;
  > for i := 0, i < 10, i := i + 1 do
  >   s := s + i
  > od;
  > s
  > EOF
  $ cat curry1.lama
  var i, s = 0;
  for i := 0, i < 10, i := i + 1 do
    s := s + i
  od;
  s
  $ ./Driver.exe -ds -llvmsm curry1.lama -o curry1.o