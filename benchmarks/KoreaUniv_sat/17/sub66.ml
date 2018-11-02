type formula = 
  True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula
  
(*let rec findVal : formula -> string list
= fun f -> 
  match f with 
  | Var (x) -> [x]
  | Neg (x) -> findVal x
  | And (p, q) -> (findVal p) @ (findVal q)
  | Or (p,q) -> (findVal p) @ (findVal q)
  | Imply (p,q) -> (findVal p) @ (findVal q)
  | Iff (p,q) -> (findVal p) @ (findVal q) 

let rec compressVal : string list -> string list
= fun sl ->
  match sl with 
  | [] -> []
  | h :: t -> 
  let rec removeVal : string list -> string -> string list -> string list
   = fun fac s l -> 
    match l with 
    | [] -> fac
    | h :: t -> if (h = s) then (removeVal fac s t) else (removeVal (fac @ [h]) h t)
  in removeVal [h] h t
*)
let rec sat : formula -> bool
= fun f -> true;;
  (*match f with 
  | True -> true
  | False -> false
  | Var (v) -> true
  | Neg (n) -> 
  (match n with
    | True -> false
    | False -> true
    | Var(v) -> true
    | Neg(nn) -> if(sat nn = false) then true
      else 
      (match nn with
      | True -> true
      | False -> false
      | Var(v) -> true
      | Neg(nnn) -> (sat nnn)
      | And(p, q) -> 
        (match (p, q) with
        | (Var(x), Var(y)) -> if(x = y) then true else false
        | (Var(x), Neg(y)) -> 
          (match y with
          | Var (z) -> if(x = z) then false else true
          | _ -> sat (And(p, q)))
        | (Neg(x),Var(y)) ->
          (match x with
           | Var (z) -> if(y = z) then false else true
           | _ -> sat (And(p,q)))
        | (Neg(x), Var(y)) -> not (sat (Or(p,q))))
      | Or(p, q) -> ((sat p) || (sat q))
      | Imply(p, q) -> 
        (
        match (p, q) with
        | (Var(x), Var(y)) -> true
        | (Var(x), Neg(y)) -> 
          (
          match y with
          | Var (z) -> if(x = z) then false else true
          | _ -> sat (Imply(p,y))
          )
        | (Neg(x),Var(y))-> true
        | (Neg(x),Neg(y))-> true
        )
      | Iff(p, q) -> 
        (match (p, q) with
        | (Var(x), Var(y)) -> true
        | (Var(x) ,Neg(y)) -> 
           (
            match y with
            | Var (z) -> if (x = z) then false else true
            | _ -> sat (Iff(p, y))
           )
        | (Neg(x), Var(y)) -> 
           (
            match x with
            | Var (z) -> if(y=z) then false else true
            | _ -> sat (Iff(x, q))
           )
        | (Neg(x),Neg(y)) -> true
        )
      )
    | And(t2,g2)-> 
      (match (t2, g2) with
      | (Var(x) ,Var(y)) -> if (x = y) then false else true
      | _ -> true
      )
    | Or(t2,g2) -> 
      (match (t2, g2) with
      | (Neg(x), Neg(y)) -> 
        ( match (x,y) with 
          | (Var z, Var k) -> if (z = k ) then false else true
          | _ -> true
        )
      | _ -> true
      )
    | Imply(t2,g2) -> 
      (match (t2, g2) with
       | (Var(x), Neg(y)) -> 
       (
        match y with
        | Var z -> if (z = x) then true else false
        | _ -> false
       )
       | _ -> if(((sat t2) = true) && (sat g2) = false) then true else false
      )
    | Iff (t2,g2) ->
      (
        match (t2,g2) with
        | (Var(x),Neg(y)) ->
          (match y with 
           | Var z -> if(x = z) then true else false
           | _ -> not (sat(t2) <> sat(g2))
          )
        | (Neg (y), Var (x)) -> 
          (match y with
           |Var z -> if(x= z) then true else false
           | _ -> not (sat(t2) <> sat (g2))
          )
        | _ -> not (sat(t2) <> sat (g2))
      )
  )
  | And (t, g) -> true
    (match t with
     | True -> if g = True then true else false
     | False-> false
     | _ -> true)
  | Or (t, g) -> true
  | Imply (t, g) -> 
    (match (t,g) with
    | (Var (x), Neg (y)) -> 
      (match y with
       | Var z -> if(z = x) then false else true
       | _ -> sat y
      )
    | _ -> true
    )
  | Iff (t, g) ->
    (match (t,g) with
     | (Var x, Neg(y)) ->
        (match y with
         | Var z -> if(z = y) then false else true
         | _ ->true
        )
     | _ -> true
    );;*)