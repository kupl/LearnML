exception Error of string
let rec sigma (a,b,f) =
  if (a > b) then (raise (Error "a > b"))
  else if (a = b) then f(a)
  else f(a) + sigma(a+1,b,f)

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
