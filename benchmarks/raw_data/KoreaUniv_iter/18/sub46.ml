let iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
 let rec iters : int * (int -> int) -> (int -> int)
 = fun (a,fn) -> 
   match a with
     | 0 -> (fun x -> x)
     | 1 -> fun x -> fn x
     | _ -> fun x -> iters((a-1), fn) (fn x)
  in iters (n,f);;
