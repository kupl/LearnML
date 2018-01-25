(*problem 1*)
# let rec pascal(col, row)=
if row = 0 || col = row then 1
else pascal (col-1, row-1) + pascal (col-1,row);;



(*problem 2*)
#let rec sigma f a b =
if a > b then 0
else (f a) + sigma f (a+1) b;;



(*problem 3*)

#let rec fold f l a =
match l with
|[] -> a
|hd::tl -> f hd(fold f tl a);;

let big x y =
if x>y then x
else y
let small x y =
if x<y thenx
else y

let max lst =
match lst with
|[] -> 0
|hd::tl -> fold big lst 1

let min lst = 
match lst with
|[] -> 0
|hd::tl -> fold small lst 10;;
