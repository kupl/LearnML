(*2014210080 Choi Kyuhyeon*)

(*Problem 1*)
let rec fastexpt b n =
match n with
|1 -> b
|_ -> if (n mod 2)=0
then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*(fastexpt b (n-1));;


  (*Problem 2*)
let rec sqrt_int guess x =
if (guess*guess-x)>0 then guess - 1
else sqrt_int (guess+1) x ;;
  
let rec div x n =
if (n mod x)=0 then x
else if (x < (sqrt_int 1 n)) then div (x+2) n
else n;;

let smallest_divisor n =
match n with
|0 -> 1
|1 -> 1
|_ -> if (n mod 2)=0 then 2
else div 3 n;;


  (*Problem 3*)
let rec iter(n,(f:int -> int)) = 
match n with
|0 -> (fun x -> x)
|1 -> f
|_ -> (fun x -> f ((iter (n-1, f) x)));;


  (*Problem 4*)
let rec product f a b =
if a<b then (f b)*(product f a (b-1))
else if a>b then (f a)*(product f (a-1) b)
else f a;;


  (*Problem 5*)
(*1. not using "product" of Problem 4*)
let rec dfact n = if (n=0)||(n=1) then 1 else n*dfact(n-2);;
  
(*2. using similar "product" of Problem 4*)
let rec product2 f a b =
if a<b then (f b)*(product2 f a (b-2))
else if a>b then (f a)*(product2 f (a-2) b)
else f a;;

let dfact n =
if (n mod 2)=0 then product2 (fun x -> x) 2 n
else product2 (fun x -> x) 1 n;;


  (*Problem 6*)
let rec drop l n = 
match l with
|[] -> []
|hd::tl -> 
if n=0 then hd::tl
else drop tl (n-1);;


  (*Problem 7*)
let fst p = match p with (x,_) -> x;;
let snd p = match p with (_,y) -> y;;

let rec concat f l =
match l with
|[] -> []
|hd::tl -> (f hd)::(concat f tl);;

let unzip l = ((concat fst l),(concat snd l));;


  (*Problem 8*)
let rec change l n =
match l with 
|[] -> 0
|hd::tl -> if n>0 then 
(if (n/hd)>0 then ((change l (n-hd)) + (change tl n))
else change tl n)
else if n=0 then 1
else 0;;



