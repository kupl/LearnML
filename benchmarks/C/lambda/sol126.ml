type lambda=V of var
|P of var * lambda
|C of lambda* lambda
and
var=string



let rec subcheck m l=
match m with
|V a -> List.mem a l
|P (a, b) ->subcheck b (a::l)
|C (a, b)->subcheck a l && subcheck b l

let check m=
subcheck m []
