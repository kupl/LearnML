(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
  match lst with
  | [] -> ([],[])
  | hd::tl ->
    let (a,b) = hd in
    let (a_list,b_list) = unzip(tl) in
    ([a]@a_list,[b]@b_list)