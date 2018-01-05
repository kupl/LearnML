(*컴퓨터공학부 2014-16775 김민지
programming language hw 1-2*)

let rec sigma ((a:int), (b:int), (f:int->int)): int =
  if a > b then 0
  else if a == b then f b
  else (f a)+ (sigma ((a+1), b, f))
