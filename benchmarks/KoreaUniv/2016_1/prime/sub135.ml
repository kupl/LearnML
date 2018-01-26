
exception Problem

let rec modul a b = 
		match b with
			2 -> a mod b <>0
			|_ -> a mod b <> 0 && modul a (b-1)


let rec prime : int -> bool
= fun n ->
			if n<2 then false else
			match n with
					2-> true
					|_ -> modul n (n-1)
