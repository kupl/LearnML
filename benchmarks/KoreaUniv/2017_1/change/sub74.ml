(*Problem 8*)
let coincost l=
  match l with
  |[]->0
  |hd::tl -> hd

let coinnext l =
  match l with
  |[]->[]
  |hd::tl -> tl

let rec change : int list -> int -> int
= fun coins amount -> if(amount = 0) then 1
  else if(amount < 0) then 0
  else if(coins=[]) then 0
  else (change coins (amount-(coincost coins))) + (change (coinnext coins) amount)



