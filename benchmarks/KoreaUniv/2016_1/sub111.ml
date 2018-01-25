(* Problem 1 *)
let rec fib n=
if n<0 then 0
else if n=0 then 0
else if n=1 then 1
else fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal(x,y)=
if x<0||y<0||y>x then 0
    else if y=0 then 1
    else if x==y then 1
    else pascal(x-1,y-1) + pascal(x-1,y);;

(* Problem 3 *)
let prime n=
    let rec recursive a b=
    if a=1 || a<0 then false
    (* 1과 음수는 소수가 아니다*)
     else if b=1 then true
      else if a mod b=0 then false
      else recursive a (b-1)
       in
       recursive n (n-1);;

(* Problem 4 *)
let rec sigma f x y=
    if y <x  then sigma f y x
    else if x==y then f x
    else f y + sigma f x (y-1);;
