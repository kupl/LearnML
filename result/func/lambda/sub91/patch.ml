type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec makePList (lambda : lambda) (pl : string list) : string list =
  match lambda with
  | V v -> pl
  | P (v, e) -> makePList e []
  | C (e1, e2) -> makePList e1 pl @ makePList e2 pl


let rec __s3 (__s4 : string) (__s5 : string list) : string list =
  match __s5 with
  | __s14 :: __s15 ->
      if __s14 = __s4 then __s3 __s4 __s15 else __s14 :: __s3 __s4 __s15
  | [] -> []


let rec makeVList (lambda : lambda) (vl : string list) : string list =
  match lambda with
  | V v -> vl @ [ v ]
  | P (v, e) -> __s3 v (makeVList e vl)
  | C (e1, e2) -> makeVList e1 vl @ makeVList e2 vl


let rec compareToPlist (pl : 'a list) e : bool =
  match pl with [] -> false | hd :: tl -> e = hd || compareToPlist tl e


let rec compareList (pl : 'b list) (vl : 'b list) : bool =
  match vl with
  | [] -> true
  | hd :: tl -> compareList pl tl && compareToPlist pl hd


let rec check (lambda : lambda) : bool =
  match lambda with
  | V v -> false
  | P (v, e) ->
      let pl : string list = makePList lambda [] in

      let vl : string list = makeVList lambda [] in
      compareList pl vl
  | C (e1, e2) -> check e1 && check e2
