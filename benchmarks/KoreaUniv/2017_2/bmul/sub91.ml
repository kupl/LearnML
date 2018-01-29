(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec digittoint l =
match l with
|[]->[]
|hd::tl -> if hd = ONE then digittoint tl@[1] else digittoint tl@[0]

let rec inttodigit l =
match l with
|[]->[]
|hd::tl -> if hd = 1 then inttodigit tl@[ONE] else inttodigit tl@[ZERO]

let rec bintoint l =
match l with
|[]->0
|hd::tl -> hd+(2*bintoint tl)

let rec inttobin n =
match n with
|0 -> []
|n -> [n mod 2]@inttobin(n/2)

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->
let a = bintoint(digittoint b1) in
let b = bintoint(digittoint b2) in
let c = (inttobin (a*b)) in
if inttodigit c=[] then [ZERO] else inttodigit c

