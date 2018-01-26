let rec prime : int -> bool

  = fun n ->

  let rec div : int -> bool

  = fun m ->

  if m * m > n then true

  else if (n mod m != 0 && div(m + 1)) then true

  else false

  in n > 1 && div 2;;

