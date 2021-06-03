let compose f g = fun x -> f(g(x));;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> match n with
  0 -> (fun x -> x)
  | _ -> compose f (iter((n-1), f));;