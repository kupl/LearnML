let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let rec recursive_function : int -> int -> int
  = fun p cnt -> match cnt with
    | 0 -> p
    | _ -> f (recursive_function p (cnt - 1)) in
      fun x -> recursive_function x n;;