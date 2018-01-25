(* problem 6*)

let drop : 'a list -> int -> 'a list

= fun l n -> 
match l with
 | [] -> raise (Failure "list is too short")
 | hd::tl -> if n<=1 then raise (Failure "not available")
 | else then hd else at (n-1) tl;;*)