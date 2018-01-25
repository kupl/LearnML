(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> 
  match l with
  | [] -> []
  | hd::tl -> (hd+n)::tl;;