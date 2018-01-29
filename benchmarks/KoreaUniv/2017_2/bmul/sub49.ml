(* problem 7*)
type digit = ZERO | ONE;;
type bin = digit list;;

let rec reverse_list lst 
  = match lst with
    | [] -> []
    | h::t -> (reverse_list t)@[h];;

let rec numToBi : int -> int -> bin = fun n x -> 
  if n = 0 then [] else (if (n mod (x * 2)) = 0 then ZERO::(numToBi n (x * 2)) else ONE::(numToBi (n - (n mod (x * 2))) (x * 2)));;

let rec biToNum : bin -> int -> int = 
  fun lst x -> 
  match lst with
  | [] -> 0
  | h::t -> if h = ONE then (x + (biToNum t (x * 2))) else biToNum t (x * 2);;

let bmul : bin -> bin -> bin =
  fun a b -> reverse_list (numToBi ((biToNum (reverse_list a) 1) * (biToNum (reverse_list b) 1)) 1);;