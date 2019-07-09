let rec num_of_digit : int -> int
= fun n ->
  if (n / 10) = 0 then 1 else 1 + (num_of_digit (n/10))

let rec power : int -> int -> int
= fun n m ->
  if m = 0 then 1 
  else if m = 1 then n
  else n * (power n (m-1))

let rec helper : int list -> int -> int
= fun lst acc ->
  match lst with
  | [] -> acc
  | hd::tl -> helper tl (acc*(power 10 (num_of_digit hd)) + hd)
  
let rec lst2int : int list -> int
= fun lst -> 
  match lst with
  | [] -> raise (Failure "Invalid Input")
  | hd::tl -> helper tl hd
;;

lst2int [2;10;124];;
lst2int [10;123];;
