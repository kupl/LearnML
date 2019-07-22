(* 컴퓨터공학과/2017-34165/김성국/1-3 *)
let rec iter (n, f) =
  if n <= 0
  then fun x -> x
  else fun x -> f ((iter (n-1, f)) x)
