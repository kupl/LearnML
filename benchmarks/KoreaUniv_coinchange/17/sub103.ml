(* 8 *)
let rec change l amt =
  let rev_list = List.rev l in
  if amt<0 then 0
  else if amt = 0 then 1
  else(
   match rev_list with
   [] -> 0
  |hd::tl -> (change tl amt) + (change l (amt-hd)));;
