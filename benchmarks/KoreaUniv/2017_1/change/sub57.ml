(* problem 8*)
  let rec change coins amount =
    match coins with
    | []->0
    | hd::tl -> if amount=0 then 1
                else if (amount<0) then 0
                else if (amount/hd)=0 then (change tl amount)
                else
                  (change tl amount) + (change coins (amount-hd));;

