(* Homework 1 - Exercise 3
 * 2011-10492 Jaeyeong Yang *)
let rec iter: int * ('a -> 'a) -> ('a -> 'a) = fun (n, f) ->
  if n <= 0 then fun x -> x
  else fun x -> iter (n - 1, f) (f x)

