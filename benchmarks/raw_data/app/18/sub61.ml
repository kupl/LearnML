let rec remove a l =
  match l with
  | [] -> []
  | hd::tl ->
    if hd=a
    then remove a tl
    else hd::(remove a tl);;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
  | [] -> l2
  | hd::tl -> app tl ((remove hd l2)@[hd]);;