  let rec drop l n =
  if n = 0 then l
  else match l with
  | [] -> []
  | hd::tl -> drop tl (n-1)