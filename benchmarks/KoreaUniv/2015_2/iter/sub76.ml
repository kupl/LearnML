let composition ((f,g) : (int -> int) * (int -> int)) (x : int) = f (g x);;

let rec iter (n,f) =
if n=0 then fun (x : int) -> x
else composition (iter (n-1,f),f);;

