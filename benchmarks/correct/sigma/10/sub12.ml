(* C:\Users\jN\Documents\����\���α׷��־��\hw1_1.ml *)

exception Error of string

let rec sigma f a b =
  if a > b then raise (Error "invalid range")
  else if a = b then f a
  else f a + sigma f (a+1) b