(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
let rec sort l =
match l with 
|[] -> []
|hd::tl ->
let rec insert a l =
match l with
|[] -> [a]
|hd::tl ->
if a > hd then a::hd::tl
else hd::(insert a tl) in
insert hd (sort tl) in
match (sort coins) with
|[] -> 0
|hd::tl ->
if amount = 0 then 1 else
if amount < 0 then 0 else
change (sort coins) (amount-hd) + change tl amount;;
