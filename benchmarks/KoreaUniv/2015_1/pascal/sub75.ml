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

(* Problem 1 *)
let rec pascal: int * int -> int
=fun (row,col) -> (* row, col *)
    match (row,col) with
        | _,0 -> 1
        | row,col when row = col -> 1
        | row,col -> pascal(row - 1,col) + pascal(row - 1,col - 1);;
