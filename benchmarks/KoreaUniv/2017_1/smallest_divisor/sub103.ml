(* 2 *)
let smallest_divisor n =
    if n mod 2 = 0 then 2
    else begin
        let squareRootOfn = sqrt (float_of_int n) in
        let intSquareRootOfn = int_of_float (floor squareRootOfn) in
        let rec check n i =
            if i <= intSquareRootOfn then (
            if n mod i = 0 then i
            else check n (i+2)
            )
            else n in
        check n 3
    end;;