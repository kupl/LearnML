(* HW5 *)
  let rec dfact a =
  if a > 2 then a * dfact (a-2)
  else a