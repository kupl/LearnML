type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check mat =
  let rec checkStringInList (li, st) =
    match li with
    | [] -> false
    | a :: remain -> if st = a then true else checkStringInList (remain, st)
  in

  let rec checkStationInArea (listOfArea, subMat) =
    match subMat with
    | V s -> checkStringInList (listOfArea, s)
    | C (m1, m2) ->
        checkStationInArea (listOfArea, m1)
        && checkStationInArea (listOfArea, m2)
    | P (a, m) ->
        let newlist = a :: listOfArea in
        checkStationInArea (newlist, m)
  in

  match mat with
  | V s -> false
  | C (m1, m2) -> m2 = m1
  | P (a, m) -> checkStationInArea ([ a ], m)
