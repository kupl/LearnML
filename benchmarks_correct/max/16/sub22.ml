(*********************)

let rec fold f l =
	match l with
	| [] -> failwith "Empty List!!"
	| [hd] -> hd
	| hd::tl ->  f hd (fold f tl);;

let big : int ->int -> int
= fun x y ->
 if x>y then x else y;;
 
let max lst = fold big lst;;
 