(* 200511843 LEE JONGHO *)

let rec sigma (a,b,f) =
	match a with
        b -> f a
	| _ -> sigma((a+1), (b+0), f) + (f a)
