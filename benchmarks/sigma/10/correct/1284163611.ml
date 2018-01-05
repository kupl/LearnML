exception Error of string

let rec sigma (a, b, f) = if (a<b) then ((f a) + (sigma ((a+1), b, f)))
                else if (a=b) then (f a)
                else (raise (Error ("index error: a shouldn't be larger than b"))) 

let square a = a * a
let identity a = a