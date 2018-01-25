(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let fst (x,_) (list1, list2) = (x::list1, list2) in
  let snd (_,y) (list1, list2) = (list1, y::list2) in
  match lst with
  | [] -> ([],[])
  |hd::[] -> fst hd (snd hd ([],[]))
  |hd::tl -> fst hd (snd hd (unzip tl));;