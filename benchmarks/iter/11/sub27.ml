let rec iter(n, f) k = 
  if n < 0 then
    raise (Invalid_argument "error")
  else if n = 0 then
    (fun x -> x) k
  else
    f (iter(n-1, f) k)

(*
let double s = s ^ s;;

print_string( iter(3, double) "a" );;
print_newline();;
*)
