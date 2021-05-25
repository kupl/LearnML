let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  match n with
    | 0 -> fun x -> x
    | _ -> fun x -> iter ((n-1),f) (f x);;
    


