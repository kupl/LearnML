(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec sat : formula -> bool
= fun f ->  
(*Var 뽑아서 (Var, true) 리스트 만들기*)
    let rec makeList : formula -> (string*bool) list -> (string*bool) list
    = fun f tab ->
        match f with
        | True -> tab
        | False -> tab
        | Var x -> 
            let rec findVar var l =
                match l with
                | [] -> ((var, true) :: tab)
                | hd :: tl -> if ( hd = (var, true)) then tab else findVar var tl
            in findVar x tab
            (*in findVar x [] []*)
        | Neg x -> (makeList x tab)
        | And (x, y)-> (makeList y (makeList x tab))
        | Or (x, y) -> (makeList y (makeList x tab))
        | Imply (x, y) -> (makeList y (makeList x tab))
        | Iff (x, y) -> (makeList y (makeList x tab))
    in
    (*리스트에서 튜플 뽑아서 bool 뒤집기*)
    let rec table boolLists =
        match boolLists with
        |[]->[]
        |(a, b)::tl ->  if b = true then ((a, false):: tl) else ((a, true)::(table tl))
    in
    (*sat 확인하기*)
    let rec satTF f bulist va = 
        match f with
        | True -> if va then true else false
        | False -> if va then false else true
        | Var x -> 
            let rec matchVar v tabl =
                match tabl with
                | hd :: tl -> (
                    if (hd = (v, va)) then true
                    else if (hd = (v, not(va))) then false
                    else matchVar v tl
                )
                | [] -> raise(Failure "Something wrong")
            in matchVar x bulist
        | Neg x -> satTF x bulist (not(va))
        | And (x, y)-> 
            if va = true then ((satTF x bulist true) && (satTF y bulist true))
            else (((satTF x bulist true) && (satTF x bulist false)) || ((satTF x bulist false) && (satTF x bulist true)) || ((satTF x bulist false) && (satTF x bulist false)))
        | Or (x, y) -> satTF (And ((Neg x), (Neg y))) bulist (not(va))
        | Imply (x, y) -> satTF (And (x, (Neg y))) bulist (not(va))
        | Iff (x, y) -> satTF (And ((Imply (x, y)), (Imply (x, y)))) bulist va
    in
    (*Var에 TF대입*)
    let rec checkTF f bultable =
        match bultable with
        | [] -> false
        |_ -> if (satTF f bultable true) then true else (satTF f (table bultable) true)
    in
    let start f boolLists = 
        match boolLists with
        | [] -> satTF f boolLists true
        |_ -> checkTF f boolLists
    in  
    start f (makeList f [])
;;