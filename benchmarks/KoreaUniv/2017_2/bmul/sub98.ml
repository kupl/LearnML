(*problem 7 *)

type digit = ZERO | ONE
type bin = digit list


let rec btoi b = match b with
|[]->0
|hd::tl -> hd+(2*btoi tl)

let rec ctob c = match c with
|[]->[]
|hd::tl -> if hd = ONE then ctob tl@[1] 
else ctob tl@[0]

let rec btoc b = match b with
|[]->[]
|hd::tl -> if hd = 1 then btoc tl@[ONE]
else btoc tl@[ZERO]

let rec itob i = match i with
|0 -> []
|a -> [a mod 2 ] @itob (a/2)

let rec bmul : bin -> bin -> bin
= fun b1 b2->
let a = btoi(ctob b1)in
let b = btoi(ctob b2)in
let c = (itob (a*b)) in btoc c
 