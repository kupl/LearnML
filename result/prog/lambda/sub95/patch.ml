type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let isinthelist (l : 'a list) a : bool =
  let temp : 'a list =
    List.filter (fun x y -> if x != y then false else true a) l
  in

  match temp with [] -> false | hd :: tl -> true


let isnotinthelist (l : 'b list) (a : string) : bool =
  let temp : 'b list =
    List.filter (fun x y -> if x != y then false else true a) l
  in

  match temp with [] -> true | hd :: tl -> false


let rec pvar (e : lambda) : string list =
  match e with
  | P (s0, e0) -> s0 :: pvar e0
  | C (e0, e1) -> pvar e0 @ pvar e1
  | V s0 -> []


let rec vvar (e : lambda) : string list =
  match e with
  | P (s0, e0) -> vvar e0
  | C (e0, e1) -> vvar e0 @ vvar e1
  | V s0 -> [ s0 ]


let rec __s1 (__s2 : lambda) (__s3 : var list) : bool =
  match __s2 with
  | V __s11 -> List.mem __s11 __s3
  | P (__s12, __s13) -> __s1 __s13 (__s3 @ [ __s12 ])
  | C (__s14, __s15) -> __s1 __s14 __s3 && __s1 __s15 __s3


let rec check (e : lambda) : bool =
  let l0 : string list = pvar e in

  let l1 : string list = vvar e in

  match e with
  | C (__s9, __s10) -> __s1 __s9 [] && __s1 __s10 []
  | P (__s7, __s8) -> __s1 e [ __s7 ]
  | V __s6 -> false
