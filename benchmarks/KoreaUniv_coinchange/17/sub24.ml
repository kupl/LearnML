(* HW8 *)
  let rec change l n =
  if n=0 then 1
  else if n < 0 then 0
  else
  match l with 
  | [] -> 0
  | hd::tl -> (change l (n-hd)) + (change tl n)
  
