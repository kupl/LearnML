type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec look : var * string list -> bool =
 fun (v, li) ->
  match li with [] -> false | h :: t -> if h = v then true else look (v, t)


let rec add : var * string list -> string list =
 fun (v, li) ->
  match li with
  | [] -> li @ [ v ]
  | h :: t -> if h = v then li else [ h ] @ add (v, t)


let rec collect1 : lambda * string list -> string list =
 fun (e, li) ->
  match e with
  | P (v, e1) -> collect1 (e1, add (v, li))
  | C (e1, e2) -> collect1 (e2, collect1 (e1, li))
  | V v1 -> li


let rec collect2 : lambda * string list -> string list =
 fun (e, li) ->
  match e with
  | P (v, e1) -> collect2 (e1, li)
  | C (e1, e2) -> collect2 (e2, collect2 (e1, li))
  | V v1 -> add (v1, li)


let rec compare : string list * string list -> bool =
 fun (li1, li2) ->
  match li2 with [] -> true | h :: t -> look (h, li1) && compare (li1, t)


let check : lambda -> bool =
 fun e ->
  let li1 = collect1 (e, []) in

  let li2 = collect2 (e, []) in
  compare (li1, li2)
