type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec look ((v : string), (li : string list)) : bool =
  match li with [] -> false | h :: t -> if h = v then true else look (v, t)


let rec add ((v : string), (li : string list)) : string list =
  match li with
  | [] -> li @ [ v ]
  | h :: t -> if h = v then li else [ h ] @ add (v, t)


let rec collect1 ((e : lambda), (li : string list)) : string list =
  match e with
  | P (v, e1) -> collect1 (e1, add (v, li))
  | C (e1, e2) -> collect1 (e2, collect1 (e1, li))
  | V v1 -> li


let rec collect2 ((e : lambda), (li : string list)) : string list =
  match e with
  | P (v, e1) -> collect2 (e1, li)
  | C (e1, e2) -> collect2 (e2, collect2 (e1, li))
  | V v1 -> add (v1, li)


let rec compare ((li1 : string list), (li2 : string list)) : bool =
  match li2 with [] -> true | h :: t -> look (h, li1) && compare (li1, t)


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let check (e : lambda) : bool =
  let li1 : string list = collect1 (e, []) in

  let li2 : string list = collect2 (e, []) in
  List.length (__s3 e) = 0
