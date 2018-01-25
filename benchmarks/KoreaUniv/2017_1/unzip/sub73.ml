(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
|[]->([],[])
|(a,b)::tl->
let first(a,b) = a in
  let second(a,b) = b in
    (first(a,b)::first(unzip tl), second(a,b)::second(unzip tl))