(* problem 6-solve*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
		|[] -> []
		|hd::tl-> if n = 0 then l
			else drop tl (n-1)