(*problem 2*)
  let rec divisor n x=if n mod x=0 then x else divisor n (x+1)
  let smallest_divisor:int->int
  =fun n->
  divisor n 2;;