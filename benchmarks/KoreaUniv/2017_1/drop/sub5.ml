(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> let rec func l n = if n=0 then l
                                       else match l with
                                       hd::tl -> func tl (n-1)
                                       | [] -> []
  in func l n