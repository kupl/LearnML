(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 1 Exercise 1  *)
let rec sigma ((a, b, f) : int * int * (int->int)) : int =
  if a > b then 0
  else f(a) + sigma (a+1, b, f)
