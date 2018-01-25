(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> let rec f l n = if n == 0 then l 
                             else match l with
                                  |[] -> []
                                  |h::t -> f t (n-1) in
             f l n
