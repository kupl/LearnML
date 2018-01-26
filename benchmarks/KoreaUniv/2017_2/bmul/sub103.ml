
  (* problem 7*)
  type digit = ZERO|ONE
  type bin = digit list

  let ten : bin -> int
  = fun t1 ->
  let rec ten1 : bin -> int
  = fun t1 ->
  match t1 with
  |[]->0
  |hd::tl->if hd==ONE then 1+2*(ten1 tl)
           else 2*(ten1 tl) in
           match List.rev t1 with
           |[]->0
           |hd::tl->if hd==ONE then 1+2*(ten1 tl)
           else 2*(ten1 tl)

  let rec bmul : bin->bin->bin
  =fun b1 b2 -> (* TODO *)
  let rec bin t1 =
  match t1 with
  |0->[ZERO]
  |1->[ONE]
  |_->if t1 mod 2=0 then (bin (t1/2))@[ZERO]
      else (bin ((t1-1)/2))@[ONE] in
      if b1=[]||b2=[] then []
       else match (ten b1)*(ten b2)  with
       |0-> [ZERO]
       |1-> [ONE]
       |_->if (ten b1)*(ten b2) mod 2 = 0 then  (bin ((ten b1)*(ten b2)/2))@[ZERO] 
           else (bin (((ten b1)*(ten b2)-1)/2))@[ONE]

