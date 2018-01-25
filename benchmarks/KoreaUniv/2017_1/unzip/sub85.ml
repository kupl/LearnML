(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
let rec map f l =
match l with
|[] -> []
|hd::tl -> (f hd)::(map f tl)
in (map fst lst, map snd lst);;
