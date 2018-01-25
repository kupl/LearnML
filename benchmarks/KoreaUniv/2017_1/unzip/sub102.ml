
(* problem 7*)
let fst (x,_) = x
let snd (_,x) = x

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
  [] -> ([],[])
| hd::tl -> let tup = unzip tl in
   (fst(hd)::fst(tup) , snd(hd)::snd(tup))
