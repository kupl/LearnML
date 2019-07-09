let rec length (lst, n) (*n = 0 in start*)
= match lst with
  |[] -> n
  |hd::tl -> length(tl, n+1);;

(*let rec hi lst2 num : (int list->int) -> int list -> int
= match lst2 with
    hd::tl -> if hd = [] then num else hi tl int(tl) *)

let rec pow(n, m) (*n의 m승*)
= match (n, m) with
  |(n, 0) -> 1
  |(n, m) -> n*pow(n, m-1);;

let hi_hd (hd :: tl) = hd;;

let rec lst2int : int list -> int
= fun lst -> (*TODO*)
match lst with
  |[] -> 0
  |hd::tl -> 
    hi_hd(lst)*pow(10, length(tl, 0)) + lst2int(tl);;

lst2int [1;2;3];;
(*
Write a function
lst2int : int list -> int
which converts a list of integers to an integer. 

For example;
lst2int [2;3;4;5] = 2345.
*)