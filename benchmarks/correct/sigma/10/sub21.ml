exception Error of string
let rec sigma f a b =
  if (a > b) then (raise (Error "a > b"))
  else if (a = b) then f(a)
  else f(a) + sigma f (a+1) b

let f1 (n) = n
let f2 (n) = n*n
(*
let _ = 
  print_int (sigma (1,100,f1));
  print_newline();
  print_int (sigma (1,100,f2));
  print_newline();
  print_int (sigma (1,1,f1));
  print_newline();
  print_int (sigma (100,1,f1));
  print_newline();*)
