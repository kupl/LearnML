(* Homework 1 - Exercise 2
 * 2011-10492 Jaeyeong Yang *)
let rec sigma f a b =
  if a > b then 0
  else f a + sigma f (a+1) b

