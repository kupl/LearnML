(* problem 3 *) 
let rec compose (f : int->int) (g : int->int) (x : int) = f(g x)
let rec all_compose n (f: int-> int) = if (n = 1) then f else compose f (all_compose (n-1) f);;
let rec iter : int *(int-> int) -> (int -> int)
= fun(n,f) -> match n with
          | 0 -> fun x -> x 
          | _ -> all_compose n f