(* 8 *)
let rec change2 l amt =
  let rev_list = List.rev l in
  if amt<0 then 0
  else if amt = 0 then 1
  else(
   match rev_list with
   [] -> 0
  |hd::tl -> (change2 tl amt) + (change2 l (amt-hd)));;
