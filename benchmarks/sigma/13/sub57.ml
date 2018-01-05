let rec sigma (a, b, f) =
  if a > b then 0 else f(b) + sigma(a, b-1, f) ;;

(* test case *)
assert(sigma(1, 10, (fun x->x)) = 55);;
assert(sigma(-1, -1, (fun x->x)) = -1);;
assert(sigma(0, 100, (fun x->x*x)) = 338350);;
assert(sigma(99,0, (fun x->x*x*x*x)) = 0);;
