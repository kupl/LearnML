(* 2015-11380 박찬양 HW1-2 *)

let rec sigma func a b =
  if a=b then func a
  else if a > b then 0
  else func a + sigma func (a+1) b
