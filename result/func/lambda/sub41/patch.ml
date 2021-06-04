type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec findStation (x : lambda) : string list =
  match x with
  | V station -> [ station ]
  | C (a, b) -> findStation a @ findStation b
  | P (area, next) -> findStation next


let rec exclude (m : lambda) (strl : string list) : bool =
  match strl with
  | [] -> true
  | _ -> (
      match m with
      | V sta -> false
      | P (area, next) ->
          if List.mem area strl then
            exclude next (List.filter (fun (x : string) -> x != area) strl)
          else exclude next strl
      | C (V sta, con2) -> exclude con2 strl
      | C (con1, V sta) -> exclude con1 strl
      | C (con1, con2) ->
          exclude con1 (findStation con1) && exclude con2 (findStation con2) )


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


let rec check (x : lambda) : bool = List.for_all __s6 (__s3 (x, []))
