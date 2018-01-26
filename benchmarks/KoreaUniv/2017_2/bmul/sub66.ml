(*problem 7*)
type digit=ZERO|ONE
type bin=digit list 
let rec length:bin->int=fun t->
match t with
|[]->0
|hd::tl -> 1+ length tl
let rec number: bin->int=fun t->
match t with
|[]->0
|hd::tl->if hd=ONE then fastexpt 2 (length(t)-1)+number tl else number tl
let rec record:int->bin->bin=fun n a->
if n=0 then [ZERO]@a
else if n=1 then [ONE]@a
else if n mod 2=1 then record ((n-1)/2) [ONE]@ a
else record (n/2) [ZERO]@ a
let bmul:bin->bin->bin=fun b1 b2->
record ((number b1)*(number b2)) []


