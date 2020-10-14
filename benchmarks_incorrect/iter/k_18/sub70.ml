let compose f g = fun x -> f(g(x))

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
    match n with
      | 0 -> f
      | 1 -> f
      | _ -> (compose f (iter(n-1, f)));;

(* Test Cases
iter(0, fun x -> 2+x) 0;;
iter(1, fun x -> 2+x) 0;;
iter(2, fun x -> 2+x) 0;;
iter(3, fun x -> 2+x) 0;;
*)
