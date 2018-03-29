  (*Problem 8*)
let rec change l n =
match l with 
|[] -> 0
|hd::tl -> if n>0 then 
(if (n/hd)>0 then ((change l (n-hd)) + (change tl n))
else change tl n)
else if n=0 then 1
else 0;;



