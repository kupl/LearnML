(*컴퓨터공학부 2014-16775 김민지
programming language hw 1-2*)

let rec sigma f a b =
  if a > b then 0
  else if a == b then f b
  else (f a)+ (sigma f (a+1) b)
