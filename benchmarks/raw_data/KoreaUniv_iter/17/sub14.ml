(* problem 3*)
let fst (x, _) = x;;

let snd (_, x) = x;;

let compose f g = fun x -> f(g(x));;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if (n = 0) then (fun x -> 1 * x) else (compose (f) (iter (n - 1, f)));;