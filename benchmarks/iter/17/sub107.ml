(* 2014 - 18474 kim ju hyeon  *)
(* 2017 fall PL 1_3 *)
let rec iter(n, f) = 
  if n<=0 then fun x -> x
  else fun x -> iter(n-1, f) (f x)
    
(* 
let _ = 
let print_int x = 
print_endline (string_of_int x) in 
  print_int ( iter (0, fun x -> 2+x) 4);
  print_int ( iter (3, fun x -> x * 2) 2);
  
 *)