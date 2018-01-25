(*problem6*)
let rec drop : 'a list->int->'a list = fun l n -> 
if (n<=0) then l
else match l with
|[]->[]
|hd::tl -> if (n=1) then tl
else drop tl (n-1);;