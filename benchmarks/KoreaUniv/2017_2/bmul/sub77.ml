
(*problem7*)
type digit = ZERO | ONE
type bin = digit list

let rec rev l=match l with
  []->[]
    |hd::tl->(rev tl)@[hd];;

let rec toDecimal:bin->int
=fun b1->match b1 with
[]->0
|hd::tl->match hd with
        ZERO->2*(toDecimal tl)
        |ONE->1+2*(toDecimal tl);;

let rec toBin:int->bin
=fun n->
if((n/2)=0) then match ((n/2)*2)=n with
  true->[ZERO]
  |false->[ONE]
else
  match ((n/2)*2)=n with
  true->(toBin (n/2))@[ZERO]
  |false->(toBin (n/2))@[ONE];;

let bmul : bin -> bin -> bin
= fun b1 b2 ->toBin((toDecimal (rev  b1))*(toDecimal (rev b2))) ;;
