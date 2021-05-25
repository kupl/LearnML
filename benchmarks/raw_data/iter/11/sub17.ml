
(* 2008-11720 Á¶°Ü¸® *)

let iter (n, f) = 
	let rec initer (n, f1, f2) =
		if (n=0) then (fun x -> x)
		else if (n=1) then f2
		else initer ((n-1), f1, (fun x -> (f1 (f2 x))))
	in
	initer (n, f, f)
