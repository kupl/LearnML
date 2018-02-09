type nat = Zero | Succ of nat;;

let rec add n1 n2 = 
  match n1 with
    Zero -> n2
  | Succ n1_minus_1 -> (add n1_minus_1 (Succ n2))
  
let rec multiply n1 n2 =
  match n1 with
    Zero -> Zero
  | Succ n1MinusOne -> add n2 (mul n1MinusOne n2 )
