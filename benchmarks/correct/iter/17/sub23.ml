(*CSE/2011-11660/Kim Jiwoo/HW1-3*)
let id x =  x (*identifier*)

let compose f g = fun x -> f (g x) (*function composition*)

let rec iter ((n:int),  f) =
    if (n==0) then id
    else compose f (iter (n-1, f))