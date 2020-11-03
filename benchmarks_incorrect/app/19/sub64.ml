let rec fold_left f a l =
  match l with
  | [] -> a
  | h::t -> fold_left f (f a h) t;;

let has_element lst e = fold_left (fun a x -> x = e || a) false lst;;

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> fold_left (fun a x -> 
    if has_element a x then a
    else a @ [x]) l2 l1;;
