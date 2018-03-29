let rec change : int list -> int -> int
= fun coins amount ->
 
let rec coin_reduce lst v =
match lst with
| [] -> 0
| hd::tl -> if (v-hd) = 0 then 1 else if (v-hd) < 0 then 0 else coin_reduce tl v + coin_reduce lst (v-hd)
 
in
 
if amount = 0 then(if coins = [] then 0 else 1)
else if amount < 0 then 0
else coin_reduce coins amount