let rec sigma ((a: int), (b: int), (f: ('a -> 'a))) : 'a =
    if (a > b) then 0
    else (sigma ((a+1), b, f)) + (f a);;
