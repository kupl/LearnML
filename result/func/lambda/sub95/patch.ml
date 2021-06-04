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


let rec __s1 __s2 (__s3 : 'c list) : 'c list =
  match __s3 with
  | [] -> __s3
  | __s13 :: __s14 ->
      if __s2 = __s13 then __s1 __s2 __s14 else __s13 :: __s1 __s2 __s14


let rec pvar (e : lambda) : string list =
  match e with
  | P (s0, e0) -> pvar e0
  | C (e0, e1) -> pvar e0 @ pvar e1
  | V s0 -> []


let rec vvar (e : lambda) : string list =
  match e with
  | P (s0, e0) -> __s1 s0 (vvar e0)
  | C (e0, e1) -> vvar e0 @ vvar e1
  | V s0 -> [ s0 ]


let rec check (e : lambda) : bool =
  let l0 : string list = pvar e in

  let l1 : string list = vvar e in

  match List.filter (isnotinthelist l0) l1 with [] -> true | hd :: tl -> false
