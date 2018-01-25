(*problem8*)
let rec change : int list->int->int = fun coins amount ->
  match coins with 
  |coin::remains->if amount<0 then 0
                    else if amount=0 then 1
                    else  change coins (amount-coin) + change remains amount
  |_->0;;