exception InvalidRange

let rec sumprod ((matrix:int*int->float), n, k) =

let rec in_pro (i, j) =
if(j = 1 ) then matrix (i, 1)
else (matrix (i,j)) *. (in_pro (i, (j-1)))

in
let rec in_sum (i,j) =
if(i = 1) then in_pro (1,j)
else (in_pro (i, j)) +. (in_sum ((i-1),j))

in
if (n < 1) then raise InvalidRange
else if ( k < 1) then raise InvalidRange
else in_sum (n, k)

