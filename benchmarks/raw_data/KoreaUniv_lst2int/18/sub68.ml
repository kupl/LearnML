let rec mylength : int list -> int
= fun lst ->
    match lst with
      | [] -> 0
      | hd::tl -> 1 + (mylength tl);;

let rec mysquare : int -> int
= fun n ->
    match n with
      | 0 -> 1
      | _ -> 10 * (mysquare (n-1));;

let rec lst2int : int list -> int
= fun lst ->
    match lst with
      | [] -> 0
      | [a] -> a
      | hd::tl -> hd * (mysquare (mylength tl)) + (lst2int tl);;

(* Test Cases
lst2int [0; 1; 2; 3; 0; 5;];;
lst2int [2;3;4;5];;
lst2int [];;
*)
