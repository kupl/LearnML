(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec twoto10 : bin -> int -> int
= fun l k ->
match l with
|[]->0
| hd::tl ->
if hd = ONE then (int_of_float (2.0**(float_of_int k))) + (twoto10 tl (k+1))
else twoto10 tl (k+1)

let rec tento2 i = 
if i=0 then []
else if i mod 2 = 1 then (tento2 (i/2))@[ONE]
else (tento2 (i/2))@[ZERO]

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->
tento2 ((twoto10 (List.rev b1) 0) * (twoto10 (List.rev b2) 0))
