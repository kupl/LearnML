 let rec  prime: int -> bool
  =fun n ->
    let n = n in
     let rec div k =
     if n = 1 then false
    else if  n = k ||  n / k < k then true
    else n mod k != 0 && div(k+1) in
      div 2;;
