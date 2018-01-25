(*problem 7*)
let unzip : ('a *'b) list -> 'a list * 'b list
= fun lst ->
let rec unzi lst =
match lst with
| [] -> ([], []) 
| (a,b)::tl ->
let (fst,snd) = unzi tl in ((a::fst), (b::snd)) in unzi lst;;