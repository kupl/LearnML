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


let rec tf boollist sen
= match sen with
|True -> true
|False -> false
|Var x-> List.assoc x boollist
|Neg x -> not(tf boollist x)
|And (a1, a2) -> (tf boollist a1) && (tf boollist a2)
|Or (a1, a2) -> (tf boollist a1) || (tf boollist a2)
|Imply (a1, a2)-> (not(tf boollist a1)) || (tf boollist a2)
|Iff (a1, a2) -> ((not(tf boollist a1)) || (tf boollist a2)) && ((not (tf boollist a2)) || (tf boollist a1))

let rec getlist f l 
= match f with
|True-> l
|False -> l 
|Var x-> (match l with
        |[]->[x]
        |hd::tl->x::(hd::tl))
|Neg x-> getlist x l 
|And (a1, a2)-> getlist a1 l  @ getlist a2 l 
|Or (a1, a2) -> getlist a1 l @ getlist a2 l 
|Imply(a1, a2) -> getlist a1 l  @ getlist a2 l 
|Iff(a1, a2) -> getlist a1 l  @ getlist a2 l 


let rec findsame l x =
match x with
|[]->l
|hd::tl-> if List.mem hd l then findsame l tl else findsame (hd :: l) tl

let rec gettruth boollist f =
match boollist with
|[]->[]
|hd::tl-> tf hd f :: gettruth tl f

let rec find boollist f =
match boollist with
|[]-> false
|hd::tl-> if hd == true then true else find tl f


let rec makelist boollist x =
match x with
|[]-> [boollist]
|v::tl-> makelist ((v, true) :: boollist) tl @ makelist((v,false)::boollist) tl


let sat : formula -> bool
= fun f->
  let flist = getlist f [] in
    let slist = findsame [] flist in
      let mlist = makelist [] slist in
        let k =  gettruth mlist f in
          find k f