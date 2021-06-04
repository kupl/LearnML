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
  a = a


let rec pvar (e : lambda) : string list =
  match e with
  | P (s0, e0) -> s0 :: pvar e0
  | C (e0, e1) -> pvar e0 @ pvar e1
  | V s0 -> []


let rec vvar (e : lambda) : string list =
  match e with
  | P (s0, e0) -> List.filter (fun (__s9 : string) -> __s9 != s0) (vvar e0)
  | C (e0, e1) -> vvar e0 @ vvar e1
  | V s0 -> [ s0 ]


let rec check (e : lambda) : bool =
  let l0 : string list = pvar e in

  let l1 : string list = vvar e in

  match List.filter (isnotinthelist l0) l1 with [] -> true | hd :: tl -> false
