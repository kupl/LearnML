exception BAD

let rec iter (n, f) = 
if n < 0 then raise BAD
else if n = 0 then (function x -> x)
else (fun x -> iter( n-1, f ) (f x ))
