(*problem2*)
let rec p2_subfunc : int->int->int->int = fun a p m->
  if p<=m && a mod p = 0 then p
  else if p<=m && a mod p != 0 then p2_subfunc a (p+1) m
  else a
let smallest_divisor : int->int = fun n->
  p2_subfunc n 2 (int_of_float(sqrt(float_of_int(n))));;
