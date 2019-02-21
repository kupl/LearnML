(*********************)
(*     Problem 1     *)
(*********************)

let rec fold f l a =
  match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)
 




let biggest = min_int

let rec max : int list -> int
= fun lst ->
	fold (fun a b -> if a>b then a else b) lst biggest

let smallest = max_int

let rec min : int list -> int
= fun lst -> 
	fold (fun a b -> if a>b then b else a) lst smallest

