  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
let check input =
let rec helpCheck a lst =
match a with
V(a) -> exists (fun a->a=a) lst
|P(a,b) -> helpCheck b (a::lst)
|C(a,b) -> helpCheck a lst && helpCheck b lst in
helpCheck input [];;


