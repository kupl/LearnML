(* =  problem 7*)
let fst(x,_) = x;;
let snd(_,x) = x;;

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
[] -> ([],[])
| h::t -> (fst(h)::fst(unzip t), snd(h)::snd(unzip t));;