type nat = ZERO | SUCC of nat;;

let rec natadd (n1,n2) = match n2 with
    | ZERO -> n1
    | SUCC n3 -> SUCC (natadd((n1,n3)))
;;

let rec natmul (n1,n2) = match n2 with
    | ZERO -> ZERO
    | SUCC n3 -> natadd((n1,natmul((n1,n3))))
;;
