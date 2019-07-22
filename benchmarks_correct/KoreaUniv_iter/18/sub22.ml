let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  match (n,f) with
    (0,_) -> (fun a -> a)
    |(_,_) -> (fun a -> iter(n-1,f) (f a));;
    