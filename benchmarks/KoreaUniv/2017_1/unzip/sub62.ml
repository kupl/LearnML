(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)
  match lst with [] -> ([],[])
 | (a,b) :: rest ->
 (match unzip rest with (flist, slist) -> (a::flist, b::slist));;
