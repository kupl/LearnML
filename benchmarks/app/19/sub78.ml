let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
  | [] -> l2
  | hd::tl -> 
    let rec search : 'a list -> 'a -> bool
    = fun l x ->
      match l with
      | [] -> false
      | hd::tl -> if hd == x then true else search tl x
    in if search l2 hd then app tl l2 else app tl (l2@[hd])
;;
(*
app [4;3;5;1;2;5;6;7] [1;2;5;4];;
*)

