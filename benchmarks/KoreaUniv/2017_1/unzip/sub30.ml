(* problem 7*)
let fst (x,_) = x
let snd (_,x) = x

let rec mkl f l =
	match l with
	| []->[]
	| hd::tl -> (f hd)::(mkl f tl)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
	(mkl fst lst, mkl snd lst)