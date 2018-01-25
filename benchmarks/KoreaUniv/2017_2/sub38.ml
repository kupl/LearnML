type btree = Empty | Node of int*btree*btree;;
let rec mirror t=
  match t with
    |Node(x1,y1,z1) -> Node(x1,mirror z1,mirror y1)
    |Empty -> Empty;;

type nat = ZERO | SUCC of nat;;
let rec natadd n1 n2=
  match n2 with 
    |SUCC y -> SUCC(natadd n1 y)
    |ZERO -> n1;;

let rec natmul n1 n2=
  match n2 with
    |ZERO -> ZERO
      |SUCC n2' -> natadd n1 (natmul n1 n2');;

let rec natexp n1 n2=
  match n2 with
    |ZERO -> SUCC ZERO
      |SUCC n2' -> natmul n1 (natexp n1 n2');;

