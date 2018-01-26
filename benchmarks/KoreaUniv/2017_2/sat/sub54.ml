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

let rec make_env x l= match x with
  |True|False->l
  |Var str->if List.exists (fun x->if x=str then true else false) l then l else str::l
  |Neg p->make_env p l
  |And (p,q)|Or (p,q)|Imply (p,q)|Iff (p,q)->make_env q (make_env p l)

let rec make_table lst l cnt = match l with
  |[]->lst
  |hd::tl->if cnt mod 2 = 1 then make_table ((hd,True)::lst) tl (cnt/2)
    else make_table ((hd,False)::lst) tl (cnt/2)

let rec interpret f l= match f with
  |Var s->let (x,v)=List.find (fun (y,z)->if y=s then true else false) l in v
  |True|False->f
  |Neg p->Neg (interpret p l)
  |And (p,q)->And ((interpret p l),(interpret q l))
  |Or (p,q)->Or ((interpret p l),(interpret q l))
  |Imply (p,q)->Imply ((interpret p l),(interpret q l))
  |Iff (p,q)->Iff ((interpret p l),(interpret q l))

let rec alu f = match f with
   True->true
  |False->false
  |Neg p->if alu p=false then true else false
  |And (p,q)->if alu p=true && alu q=true then true else false
  |Or (p,q)->if alu p=false &&alu q=false then false else true
  |Imply (p,q)->if alu p=false then true else alu q
  |Iff (p,q)->if alu p=alu q then true else false


let rec sat : formula -> bool
= fun f ->
  let varlist = make_env f [] in
  let length=int_of_float (2.0 ** (float_of_int (List.length varlist))) in
  let rec cnt_sat i = if alu (interpret f (make_table [] varlist i))=true then true else
    if i+1=length then false
    else cnt_sat (i+1) in cnt_sat 0

