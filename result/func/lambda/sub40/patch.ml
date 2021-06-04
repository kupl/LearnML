type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (met : lambda) : bool =
  let rec checkIn ((lambda : lambda), (stationlist : string list)) : bool =
    match lambda with
    | V stationstr -> if List.mem stationstr stationlist then true else false
    | P (areastr, metin) -> checkIn (metin, stationlist @ [ areastr ])
    | C (metin1, metin2) ->
        checkIn (metin1, stationlist) && checkIn (metin2, stationlist)
  in

  match met with V varstr -> false | _ -> checkIn (met, [])
