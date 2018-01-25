(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
  match lst with
  | [] -> ([],[])
  | (x,y)::tl -> let l1, l2 = unzip tl in x::l1, y::l2;;