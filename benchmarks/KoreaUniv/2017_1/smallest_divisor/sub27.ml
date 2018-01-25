(*problem 2 *) let rec div: int->int->int=fun a n-> if a*a <=n then (if n mod a=0 then a else div (a+1) n) else n
let smallest_divisor: int ->int =fun n -> div 2 n 
