(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
let fst p = match p with (x,_) -> x in
let snd p = match p with (_,x) -> x in
let rec unzip_prime = fun tl c d ->
match tl with
[] -> (c,d)
|h::t -> unzip_prime t (c@[fst h]) (d@[snd h])
 in unzip_prime lst [] []
