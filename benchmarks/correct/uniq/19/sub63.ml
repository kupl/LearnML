let rec fold_left f a l =
  match l with
  | [] -> a
  | h::t -> fold_left f (f a h) t;;

let rec has_element lst e =
	match lst with
	| [] -> false 
	| hd::tl -> (hd = e)||(has_element tl e);;

let uniq : 'a list -> 'a list
= fun lst -> fold_left (fun a x -> if has_element a x then a else a @ [x]) [] lst;;