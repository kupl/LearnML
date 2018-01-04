(* C:\Users\saigoy\Desktop\iter.ml *)

let rec iter : int * ('a -> 'a) -> ('a -> 'a) = fun (n , f) init ->
  if (n > 0) then (f (iter ((n-1) , f) init))
  else init;;

