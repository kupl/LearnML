type nat = ZERO | SUCC of nat

let rec natadd n1 n2=
   match n1 with
      |ZERO->n2
      |SUCC(nat)->SUCC(natadd nat n2);;   


let rec natmul n1 n2=
   match n1 with
      |ZERO->ZERO
      |SUCC(ZERO)->n2
      |SUCC(nat)->natadd (natmul nat n2) n2;;



   