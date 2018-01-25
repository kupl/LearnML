(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun ('a * 'b) lst -> 
  match lst with
  | [] -> []
  | hd::tl -> fun ('a * 'b) tl
  | length[lst]=1 -> ([a],[b]);; 