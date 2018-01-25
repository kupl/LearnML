(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
        if a=b then f a
        else (f a) * (product f (a+1) b)

(* problem 5*)

let dfact : int -> int
= fun n -> 
        match n mod 2 with
        | 0 -> product (fun x->2*x) 1 (n/2)
        | 1 -> product (fun x->2*x-1) 1 ((n+1)/2)
        | _ -> 0