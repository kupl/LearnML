type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string
;;

let check : lambda -> bool = fun met ->
  let rec checkIn : lambda * var list -> bool = fun (lambda, stationlist) ->
    match lambda with
    | V(stationstr) -> if (List.mem stationstr stationlist) then true else false
    | P(areastr,metin) -> checkIn (metin,stationlist@[areastr] )
    | C(metin1,metin2) -> (checkIn (metin1,stationlist))&&(checkIn (metin2,stationlist))
  in
  match met with
  | V(varstr) -> true
  | _ -> checkIn(met,[])
;;
