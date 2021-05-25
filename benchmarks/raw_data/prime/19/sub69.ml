let rec f1 n a =
    match a with
      | 1 -> true 
      |_  ->  match n mod a with
        | 0 -> false
        |_ -> true && (f1 n (a-1));;

let prime : int -> bool
= fun n -> 
  if n < 2 then false 
  else
    match n with
      | 2 -> true
      |_  -> f1 n (n-1);;