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

let rec scan f varlist = 
  match f with
  | Var x -> (
              match varlist with 
              | [] -> x::[]
              | hd::tl-> if hd=x then varlist else (scan f tl)@varlist
              )
  | Neg a -> scan a varlist
  | And (a1,a2) -> scan a2 (scan a1 varlist)
  | Or (a1,a2) -> scan a2 (scan a1 varlist)
  | Imply (a1,a2) -> scan a2 (scan a1 varlist)
  | Iff (a1,a2) -> scan a2 (scan a1 varlist)
  | _-> varlist

let rec finList hd posslist = 
  match posslist with 
  |[]->[]
  |ph::pt -> ((hd,true)::ph)::((hd,false)::ph)::(finList hd pt)

let rec makeEnv varlist posslist= 
  match varlist with 
  | [] -> posslist
  | hd::tl -> (
              match posslist with
              |[]->(makeEnv tl [[(hd,true)];[(hd,false)]])
              |ph::pt-> makeEnv tl (finList hd posslist)
              )


let rec apply_env x env =
  match env with
  | [] -> false
  | (y,v)::tl -> if x = y then v else (apply_env x tl)

let rec resolve f givenEnv =
  match f with
  | True -> true
  | False -> false
  | Var x -> apply_env x givenEnv
  | Neg a -> if (resolve a givenEnv) = true then false else true
  | And (a1,a2) ->(let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (b1&&b2))
  | Or (a1,a2) ->(let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (b1||b2))
  | Imply (a1,a2) -> (let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (if (b1=true)&&(b2=false) then false else true)) 
  | Iff (a1,a2) -> (let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (b1=b2))


let rec untiltrue f envlist =
  match envlist with
  | []->resolve f []
  | hd::tl -> if (resolve f hd)=true then true else (untiltrue f tl)


let rec sat : formula -> bool
= fun f -> (* TODO *)
    let vars = scan f [] in
      let envlist = makeEnv vars [] in
          untiltrue f envlist
