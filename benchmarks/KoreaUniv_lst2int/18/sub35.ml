let getdig n = int_of_float(log10 (float_of_int(n))) + 1;;

let rec reallength: int list -> int
= fun l ->
  match l with
    [] -> 0
    |h::t -> getdig h + reallength t;;

let rec power10 n =
  match n with
    0 -> 1
    |_ -> 10 * power10 (n-1);;
    
let rec lst2int : int list -> int
= fun lst ->
  match lst with
    [] -> 0
    |h::t -> if t = [] then h else h*(power10 (reallength t)) + lst2int t;;

