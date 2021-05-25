let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> []
  | hd::tl ->
    let rec remove : 'a list -> 'a -> 'a list
    = fun l x ->
      match l with
      | [] -> []
      | hd::tl -> if hd == x then remove tl x else hd::(remove tl x)
    in hd::(uniq (remove tl hd))
;;
(*
uniq [5;5;5;6;5;4;1;3;3;2;6;5;4;1;4;5;32;];; 
*)