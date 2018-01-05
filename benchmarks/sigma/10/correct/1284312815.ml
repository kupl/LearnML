(* C:\Users\jN\Documents\����\���α׷��־��\hw1_1.ml *)

exception Error of string

let rec sigma (a, b, f) =
  if a > b then raise (Error "invalid range")
  else if a = b then f a
  else f a + sigma ((a + 1), b, f)

let _ = print_int (sigma (3, 3, (fun x -> 2 * x)));;