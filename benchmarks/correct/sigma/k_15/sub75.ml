(* Helper functions *)
let rec map f l =
    match l with
    | [] -> []
    | head::tail -> (f head)::(map f tail);;

let rec fold f l acc =
    match l with
    | [] -> acc
    | head::tail -> f head (fold f tail acc);;

let helper start halt = 
    let rec aux n acc =
        if n < start then acc else aux (n - 1) (n::acc)
    in aux halt [];;

let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
    fold (fun x y -> x+y) (map f (helper a b)) 0;;

