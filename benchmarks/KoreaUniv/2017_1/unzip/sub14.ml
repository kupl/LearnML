(* problem 7*)
let rec proc2 f l = match l with |[] -> []|hd::tl -> (f hd)::(proc2 f tl);;

let combine l1 l2 = (l1, l2);;

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> combine (proc2 fst lst) (proc2 snd lst);;