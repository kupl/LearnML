(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec fastexpt : int -> int -> int
= fun b n ->
if n=0 then 1
else if (n mod 2)=0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
else (fastexpt b (n/2)) * (fastexpt b (n/2)) * b

let rec count : bin -> int
= fun a ->
match a with
[]->0
|h::t -> 1+count t

let rec decimalize : bin -> int -> int
= fun a b ->
match a with
[]->0
|h::t-> if(h=ONE) then (fastexpt 2 (b-1)) + (decimalize t (b-1)) else (decimalize t (b-1))

let rec binarylize : int -> bin -> bin
= fun a b ->
match a with
0 -> b@[]
|_ -> if((a mod 2) = 1) then (binarylize (a/2) b@[ONE]@b) else (binarylize (a/2) b)@[ZERO]@b

let bmul : bin -> bin -> bin
= fun b1 b2 ->
binarylize (decimalize b1 (count b1) * decimalize b2 (count b2)) []


