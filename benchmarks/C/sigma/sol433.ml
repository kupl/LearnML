(* 컴퓨터공학과/2017-34165/김성국/1-2 *)
let rec sigma f a b =
  if a > b
  then 0
  else (f a) + sigma f (a+1) b
