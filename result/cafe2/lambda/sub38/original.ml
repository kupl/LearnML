type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec contains (listOfArea : 'a list) s : bool =
  match listOfArea with
  | [] -> false
  | h :: t -> if h = s then true else contains t s


let rec check2 (x : lambda) (listOfArea : string list) : bool =
  match x with
  | V s -> contains listOfArea s
  | P (n, m) -> check2 m (n :: listOfArea)
  | C (m1, m2) -> check2 m1 listOfArea && check2 m2 listOfArea


let rec check (x : lambda) : bool =
  match x with V s -> true | _ -> check2 x []
