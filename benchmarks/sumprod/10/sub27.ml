exception Error of string

let matrix (i, j) = (float_of_int)(i+j)

let rec sigma (a, b, f) = if (a<b) then ((f a) +. (sigma ((a+1), b, f)))
                else if (a=b) then (f a)
                else (raise (Error ("index error: a shouldn't be larger than b")))

let rec bigpi (a, b, f) = if (a<b) then ((f a) *. (bigpi ((a+1), b, f)))
                else if (a=b) then (f a)
                else (raise (Error ("index error: a shouldn't be larger than b")))
                
let rec sumprod (f, n, k) = if (n<=0 || k <=0) then (raise (Error "n and k souldn't be smaller than 1"))
else sigma (1, n, (function x -> bigpi (1, k, (function y -> (f (x, y))))))                