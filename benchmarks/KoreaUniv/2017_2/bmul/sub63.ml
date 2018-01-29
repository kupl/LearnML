(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin = fun b1 b2 ->
let shift : bin -> bin = fun b -> b @ [ZERO] in
let rec add : bin -> bin -> digit -> bin -> int -> bin = fun p q r s i ->
if ( List.length p >= i && List.length q >= i ) then
match (List.nth p ((List.length p) - i), List.nth q ((List.length q) - i), r) with
|(ONE, ONE, ONE) -> add p q ONE (ONE::s) (i+1)
|(ONE, ONE, ZERO) -> add p q ONE (ZERO::s) (i+1)
|(ONE, ZERO, ONE) -> add p q ONE (ZERO::s) (i+1)
|(ZERO, ONE, ONE) -> add p q ONE (ZERO::s) (i+1) 
|(ZERO, ZERO, ZERO) -> add p q ZERO (ZERO::s) (i+1)
|(ONE, ZERO, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ONE, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ZERO, ONE) -> add p q ZERO (ONE::s) (i+1)
else if(List.length p >= i) then
match (List.nth p ((List.length p) - i), r) with
|(ONE, ONE) -> add p q ONE (ZERO::s) (i+1)
|(ONE, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ONE) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ZERO) -> add p q ZERO (ZERO::s) (i+1)
else if(List.length q >= i) then
match (List.nth q ((List.length q) - 1), r) with
|(ONE, ONE) -> add p q ONE (ZERO::s) (i+1)
|(ONE, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ONE) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ZERO) -> add p q ZERO (ZERO::s) (i+1)
else if (r = ONE) then (ONE::s) else s
in let rec mul : bin -> bin -> int -> int -> bin -> bin = fun p q len i s ->
if(len>=i&&List.nth q ((List.length q)-i) = ONE) then mul (shift p) q len (i+1) (add p s ZERO [] 1)
else if(len>=i&&List.nth q ((List.length q)-i) = ZERO) then mul (shift p) q len (i+1) s
else s
in if(b1 = [ZERO] or b2 = [ZERO]) then [ZERO] else mul b1 b2 (List.length b2) 1 [];;
