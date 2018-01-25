(* problem 6*)
let rec drop: 'a list -> int -> 'a list
    = fun l n -> 
        match l with
        | [] -> []
        | hd :: tl -> match n with
                    | 0 -> l
                    | _ -> drop tl (n-1);;