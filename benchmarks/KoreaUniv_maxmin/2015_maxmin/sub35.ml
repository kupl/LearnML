let rec fold f l a =
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
