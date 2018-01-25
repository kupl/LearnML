(* Helper functions *)
let rec map f l =
    match l with
    | [] -> []
    | head::tail -> (f head)::(map f tail);;

let rec fold f l acc =
    match l with
    | [] -> acc
    | head::tail -> f head (fold f tail acc);;

let (--) start halt = 
    let rec aux n acc =
        if n < start then acc else aux (n - 1) (n::acc)
    in aux halt [];;

let rec max : int list -> int
=fun l -> match l with
    | [e] -> e
    | h::t when h >= max t -> h
    | h::t -> max t
    | _ -> 0;;


let rec min : int list -> int
=fun l -> match l with
    | [e] -> e
    | h::t when h <= max t -> h
    | h::t -> min t
    | _ -> 0;;
