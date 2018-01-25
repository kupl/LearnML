
(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  match lst with
  |[]->([],[])
  |hd::tl ->
  let (x,y)=hd
    in let (p,q) = unzip tl
      in (x::p, y::q)
