(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
    match l with
    | [] -> []
    | hd::t1 -> if n=0 then l
                else drop t1 (n-1)