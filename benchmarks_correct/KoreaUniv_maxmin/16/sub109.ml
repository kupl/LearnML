let rec fold f l a =
 match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)

let greater a b =
	if a>=b then a
	else b

let rec max : int list -> int
= fun lst -> 
    match lst with
      | hd::tl -> fold greater tl hd

