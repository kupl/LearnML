type nat = ZERO | SUCC of nat;;

let rec natadd (n1, n2) = 
(*val natadd : nat * nat -> nat*)
  match n2 with
  | ZERO -> (match n1 with
            | ZERO -> ZERO
            | SUCC(n1prime) -> SUCC(natadd (n1prime, ZERO)))
  | SUCC(n2prime) -> (match n1 with
                    | ZERO -> SUCC(natadd (n2prime, ZERO))
                    | SUCC(n1prime) -> SUCC(SUCC(natadd (n1prime, n2prime))));;
  

let rec natmul (n1, n2) =
(*val natmul : nat * nat -> nat*)
  match n2 with 
  | ZERO -> ZERO
  | SUCC(n2prime) -> (match n1 with
                    | ZERO -> ZERO
                    | SUCC(n1prime) -> natadd (natadd(natmul(n1prime,n2prime),n1prime), natadd(n2prime,SUCC(ZERO))));;
