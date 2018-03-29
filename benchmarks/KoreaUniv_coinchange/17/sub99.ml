(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
let rec insert a l = match l with
|[] -> [a]
|hd::tl -> if a > hd then a::hd::tl else hd::insert a tl in
 let rec sort l = match l with
 |[] -> []
 |hd::tl -> insert hd (sort tl) in
  let rec sub l a = match l with
  |[] -> 0
  |hd::tl -> match a with
   |0 -> 1
   |_ -> if a < 0 then 0 else sub l (a-hd) + sub tl a
   in sub (sort coins) amount;;

