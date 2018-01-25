(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
 match lst with
 | [] ->([],[])
 | (h1,h2)::t -> let (t1, t2) = unzip t in (h1::t1, h2::t2)