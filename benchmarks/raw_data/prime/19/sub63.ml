let rec prime_internal : int -> int -> bool
  = fun n i ->
    if n = i then true
    else if n mod i = 0 then false
    else prime_internal n (i+1)

let rec prime : int -> bool
  = fun n ->
    if n = 1 then false
    else prime_internal n 2;;
    
    
prime 0;;
prime 1;;
prime 2;;
prime 3;;
prime 4;;
prime 5;;
prime 6;;
