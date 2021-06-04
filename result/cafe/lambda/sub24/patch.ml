type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (mat : lambda) : bool =
  let rec checkStringInList ((li : string list), (st : string)) : bool =
    match li with
    | [] -> false
    | a :: remain -> if st = a then true else checkStringInList (remain, st)
  in

  let rec checkStationInArea ((listOfArea : string list), (subMat : lambda)) :
      bool =
    match subMat with
    | V s -> checkStringInList (listOfArea, s)
    | C (m1, m2) ->
        checkStationInArea (listOfArea, m1)
        && checkStationInArea (listOfArea, m2)
    | P (a, m) ->
        let newlist : string list = a :: listOfArea in
        checkStationInArea (newlist, m)
  in

  match mat with
  | V s -> false
  | C (m1, m2) -> check m1 && check m2
  | P (a, m) -> checkStationInArea ([ a ], m)
