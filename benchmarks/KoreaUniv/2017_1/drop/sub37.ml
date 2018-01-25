(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
   match l with
     [] -> [] (*when l is empty, just return an empty list*)
     |hd::tl -> (*when l is not empty, call the function "drop" recursively until n becomees 0*)
	if n = 0 then l else drop tl (n - 1);;