
(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
        match n with
        | 0 -> l
        | _ -> match l with
                | [] -> l
                | hd::tl -> drop tl (n-1)