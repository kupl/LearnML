(*8*)

  let change : int list -> int -> int
  =fun coins amount ->
  
  (*let  rec nth l n =
   match l with 
   |[]->raise
   |hd::tl -> if n=0 then hd else nth tl (n-1) 
  in
  *)
  let rec length l=
  match l with
  |[]->0
  |hd::tl-> 1+length tl
  in
 
  let amount = amount -( amount mod 5) in
   let rec counter coins amount n =
  if amount =0 then 1
else if amount <0 then 0
else if n=length coins then 0
else ((counter coins (amount - (List.nth coins n)) n)+(counter coins amount (n+1)))
in counter coins amount 0
;;


