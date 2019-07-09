
let rec length l =
  match l with
    []-> 1
    |_::t-> 10 * length t;;

let rec lst2int : int list -> int
= fun lst -> match lst with
  [] -> 0
  |h::t -> (h* length lst/10) + lst2int t;;



lst2int [2;3;4;5];;
