exception NonsenseArg

let rec length l =
  match l with
    [] -> 0
  | h::t -> 1 + length t;;

let rec exponent n k =
  match k with
    0 -> 1
  | m -> n * exponent n (k-1);;

let rec lst2int l =
  let n = length l in
    match l with
      [] -> raise NonsenseArg
    | [x] -> x
    | h::t -> h * (exponent 10 (n-1)) + lst2int t;;

lst2int [2; 3; 4; 5];;
lst2int [1; 2; 3; 4; 5; 6; 7];;
(* lst2int [];; *)
