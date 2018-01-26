(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec chtobin l =
match l with
|[]->[]
|hd::tl -> if hd = ONE then chtobin tl@[1] else chtobin tl@[0]

let rec bintoch l =
match l with
|[]->[]
|hd::tl -> if hd = 1 then bintoch tl@[ONE] else bintoch tl@[ZERO]

let rec bintoint l=
match l with
|[]->0
|hd::tl->hd+(2*bintoint tl)

let rec inttobin a= 
match a with
  |0-> []
  |a-> [a mod 2] @ inttobin (a/2)


let rec bmul : bin->bin->bin
= fun b1 b2 -> 
let a = bintoint(chtobin b1)in
let b = bintoint(chtobin b2) in
let c = (inttobin (a*b)) in bintoch c
