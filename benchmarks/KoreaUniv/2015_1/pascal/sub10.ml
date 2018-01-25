exception WrongInput

let pascal : int * int -> int
=fun (x,y) -> let rec factorial x=match x with
1 -> 1
|_ -> x * factorial(x-1)
in if y<0||x<0||y>x then raise WrongInput
else if y=0||x=y then 1 else factorial x/((factorial y)*factorial (x-y));;

