let rec find_root a n = if (a > (n*n)) then (find_root a (n+1)) else n;;
let rec find_divisor a n = if ((a mod n) = 0) then n else (if (n > (find_root a 1)) then a else (find_divisor a (n+1)));;

let smallest_divisor : int -> int = fun a -> find_divisor a 2;;