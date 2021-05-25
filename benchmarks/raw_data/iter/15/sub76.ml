let rec makefunc n f result =
    if n == 0 then
        result
    else
        makefunc (n - 1) f (f result)

let iter (n, f) = makefunc n f
