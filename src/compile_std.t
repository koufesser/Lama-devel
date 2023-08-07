  $ lamac -ds "../../../../../stdlib/Array.lama"
  $ ./Driver.exe -sml Array.sm
  $ cp output.o Array.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Buffer.lama"
  $ ./Driver.exe -sml Buffer.sm
  $ cp output.o Buffer.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Data.lama"
  $ ./Driver.exe -sml Data.sm
  $ cp output.o Data.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Fun.lama"
  $ ./Driver.exe -sml Fun.sm
  $ cp output.o Fun.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Lazy.lama"
  $ ./Driver.exe -sml Lazy.sm
  $ cp output.o Lazy.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/List.lama"
  $ ./Driver.exe -sml List.sm
  $ cp output.o List.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Matcher.lama"
  $ ./Driver.exe -sml Matcher.sm
  $ cp output.o Matcher.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Ostap.lama"
  $ ./Driver.exe -sml Ostap.sm
  $ cp output.o Ostap.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Random.lama"
  $ ./Driver.exe -sml Random.sm
  $ cp output.o Random.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Ref.lama"
  $ ./Driver.exe -sml Ref.sm
  $ cp output.o Ref.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/STM.lama"
  $ ./Driver.exe -sml STM.sm
  $ cp output.o STM.o
  $ llc output.ll -o output1.s

  $ lamac -ds "../../../../../stdlib/Timer.lama"
  $ ./Driver.exe -sml Timer.sm
  $ cp output.o Timer.o
  $ llc output.ll -o output1.s

  $ clang -no-pie stdlib.o Array.o Buffer.o Data.o Fun.o Lazy.o List.o Matcher.o Ostap.o Random.o Ref.o STM.o Timer.o || echo $?
  $ cp ./a.out "../../../../../src/stdlib.o"