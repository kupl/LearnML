let iter : int * (int -> int) -> (int -> int)
= fun (n,f) m -> 
  let rec loop n f =
    if n < 2 then f m
    else f m + (loop (n-1) f) in
    loop n f;;

iter (12, fun x -> 2+x) 2;;
