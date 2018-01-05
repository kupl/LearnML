(* sigma : int * int * (int -> int) -> int *)
(**
let rec sigma_naive (a,b,f) =
  if (a>b) then 0
  else f(a) + sigma_naive(a+1, b, f)
**)

let sigma (a,b,f) =
  let rec aux (a,b,f, acc) =
    if (a>b) then acc
    else aux(a+1, b, f, acc + f(a))
  in

  aux (a,b,f, 0)


(**
print_int (sigma(1,10, fun x->x*x))
**)
