(* 컴퓨터공학과/2017-34165/김성국/1-2 *)
let rec sigma (a, b, f) =
  if a > b
  then 0
  else (f a) + sigma (a+1, b, f)
