type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s1 (__s2 : lambda) (__s3 : string list) : bool =
  match __s3 with
  | [] -> (
      match __s2 with
      | V __s6 -> false
      | P (__s7, __s8) -> __s1 __s8 (__s7 :: __s3)
      | C (__s9, __s10) ->
          if __s1 __s9 __s3 = true && __s1 __s10 __s3 = true then true
          else false )
  | __s11 :: __s12 -> (
      match __s2 with
      | V __s13 -> if __s13 = __s11 then true else __s1 __s2 __s12
      | P (__s14, __s15) -> __s1 __s15 (__s14 :: __s3)
      | C (__s16, __s17) ->
          if __s1 __s16 __s3 = true && __s1 __s17 __s3 = true then true
          else false )


let check (input : lambda) : bool =
  let rec helpCheck (a : lambda) (lst : string list) : bool =
    match a with
    | V a -> List.exists (fun (a : string) -> a = a) lst
    | P (a, b) -> helpCheck b (a :: lst)
    | C (a, b) -> helpCheck a lst && helpCheck b lst
  in
  __s1 input []
