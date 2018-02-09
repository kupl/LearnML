(*********************)
(*     Problem 1     *)
(*********************)

let rec max  : int list -> int
= fun lst -> 
    match lst with
    |[] -> 0
    |hd::tl -> if hd > (max tl) then hd else (max tl) ;;

(* TODO *)


let rec min : int list -> int
= fun lst ->  
    match lst with
    |[] -> raise(Failure "No empty list can be accepted")
    |[a] -> a 
    |hd::tl -> if hd < (min tl) then hd else (min tl);; 
(* TODO *)
