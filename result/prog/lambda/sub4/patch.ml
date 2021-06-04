type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 ((__s4 : lambda), (__s5 : string list)) :
    (lambda * string list) list =
  match (__s4, __s5) with
  | P (__s16, __s17), __s18 -> __s3 (__s17, __s16 :: __s18)
  | V __s19, __s20 -> [ (V __s19, __s20) ]
  | C (__s21, __s22), __s23 ->
      List.append (__s3 (__s21, __s23)) (__s3 (__s22, __s23))


let __s6 (__s7 : lambda * string list) : bool =
  match __s7 with
  | V __s8, __s9 -> List.mem __s8 __s9
  | P (__s10, __s11), __s12 -> false
  | C (__s13, __s14), __s15 -> false


let rec check (m : lambda) : bool =
  let rec append (m1 : string list) (m2 : string list) : string list =
    match m1 with
    | h :: t -> if List.mem h m2 then append t m2 else append t (h :: m2)
    | [] -> m2
  in

  let rec check2 (m : lambda) (l : string list) : string list =
    match m with
    | V n -> l
    | P (n, m2) -> if List.mem n l then check2 m2 l else check2 m2 (n :: l)
    | C (m1, m2) -> append (check2 m1 l) (check2 m2 l)
  in

  let rec check3 (m : lambda) (l : string list) : bool =
    match m with
    | V n -> if List.mem n l then true else false
    | P (n, m2) -> check3 m2 l
    | C (m1, m2) -> check3 m1 l && check3 m2 l
  in

  let l : string list = check2 m [] in
  List.for_all __s6 (__s3 (m, []))
