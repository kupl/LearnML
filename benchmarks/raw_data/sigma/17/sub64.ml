let rec sigma ((a:int),(b:int),(f:int->int)) : int = 
if (a>b) then  0
else if(a==b) then (f (a))
else ((f a) + (sigma(a+1,b,f)))



(*test
let _ =
let print_bool x = 
print_endline(string_of_bool x) in
print_bool(385 = sigma(1,10,(fun x -> x*x)));
print_bool(0=sigma(3,1,fun x-> x*x));
print_bool(27 = sigma(3,3,fun x -> x*x*x));
print_bool(385=sigma(-10,-1,fun x->x*x))
*)
