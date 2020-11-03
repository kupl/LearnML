let rec fold_left f a l =
  match l with
  | [] -> a
  | hd::tl -> fold_left f (f a hd) tl;;

(* check duplicate *)
let rec check : 'a -> 'a list -> bool
= fun n l ->
  match l with
  | [] -> true
  | hd::tl -> if n=hd then false else check n tl;;

(* if not duplicate -> add element *)  
let make_list : 'a list -> 'a -> 'a list
= fun l n ->
  if check n l then (l@[n]) else l;;  

let uniq : 'a list -> 'a list
= fun lst -> 
    fold_left make_list [] lst;;

(*
uniq [5;6;5;4];;
uniq [2;2;6;2;6;3;6;1];;
*)