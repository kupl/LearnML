let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
  match n with
    0 -> (fun x-> x)
    |1 -> (fun x -> (f x))
    | _ -> (fun x -> f (iter (n-1, f) x));;
    
iter (3, fun x -> 2+x) 4;;