(** f : 'a -> 'a *)
(**
let rec iter_naive (n, f) =
  if (n<=0) then (fun x -> x)
  else (fun x -> f (iter_naive(n-1,f) x))
;;
**)
let iter (n,f) =
  let rec aux (n,f, x, acc)  =
    if (n<=0) then acc
    else aux(n-1, f, x, f acc)
  in

  fun x -> aux (n,f,x,x)
