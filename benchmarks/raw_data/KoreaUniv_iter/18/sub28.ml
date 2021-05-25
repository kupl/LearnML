let rec recf n x f = if n=1 then f x else f(recf (n-1) x f);;
let iter : int * (int -> int) -> (int -> int) = fun (n,f) -> if n=0 then (fun x -> x) else (fun x -> recf n x f);;