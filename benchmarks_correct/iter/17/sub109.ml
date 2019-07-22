let rec iter (n, f)=
  if (n==0) 
  then (fun x->x)
  else (fun x-> f ( iter((n-1), f) x) )
(*
let test_f x=
  2+x

let _ = print_int ( (iter (0, test_f) 8)) ; print_endline("")
let _ = print_int ( (iter (1, test_f) 8)); print_endline("")
let _ = print_int ( (iter (5, test_f) 8)); print_endline("")
*)
