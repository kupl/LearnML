(* problem 5*)
let dfact: int -> int
= fun n -> let rec product: (int -> int) -> int -> int -> int
    = fun f a b -> if a = b then f a 
    else (f b) * (product f a (b-1)) in 
        match n mod 2 with
        | 0 -> product (fun x -> 2*x) 1 (n/2)
        | _ -> product (fun x -> 2*x - 1) 1 ((n+1)/2);;