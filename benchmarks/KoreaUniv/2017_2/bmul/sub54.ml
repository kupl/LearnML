(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec baddd b1 b2 c = match (b1,b2) with
  |([],[])->if c=ZERO then [] else [ONE]
  |([],l)|(l,[])->if c=ZERO then l else baddd [c] l ZERO
  |(l1,l2)->let t1=(List.length l1-1) in let t2=(List.length l2-1) in
  let new_c = match (List.nth b1 t1, List.nth b2 t2, c) with
    |(ONE,ONE,ONE)|(ONE,ONE,ZERO)|(ONE,ZERO,ONE)|(ZERO,ONE,ONE)->ONE
    |_->ZERO
  in let s = match (List.nth b1 t1, List.nth b2 t2, c) with
    |(ONE,ZERO,ZERO)|(ZERO,ONE,ZERO)|(ZERO,ZERO,ONE)|(ONE,ONE,ONE)->ONE
    |_->ZERO
  in let cutter b = List.rev (List.tl (List.rev b))
  in (baddd (cutter b1) (cutter b2) new_c)@[s]

let rec badd b1 b2 = baddd b1 b2 ZERO

let rec bshl b n = match n with
  |0->b
  |_->bshl (b@[ZERO]) (n-1)

let rec cntmul x y res i = if i=(-1) then res
  else match x with
  |[]->res
  |hd::tl->if hd=ONE then (cntmul tl y (badd res (bshl y i)) (i-1)) else (cntmul tl y res (i-1))

let bmul : bin -> bin -> bin
= fun b1 b2 -> cntmul b1 b2 [ZERO] ((List.length b1) -1)
