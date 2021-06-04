type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) (__s5 : var list) : bool =
  match __s4 with
  | V __s6 -> List.mem __s6 __s5
  | P (__s7, __s8) ->
      let __s9 : var list = __s7 :: __s5 in
      __s3 __s8 __s9
  | C (__s10, __s11) -> __s3 __s10 __s5 && __s3 __s11 __s5


let check (input : lambda) : bool =
  let rec helpCheck (a : lambda) (lst : string list) : bool =
    match a with
    | V a -> List.exists (fun (a : string) -> a = a) lst
    | P (a, b) -> helpCheck b (a :: lst)
    | C (a, b) -> helpCheck a lst && helpCheck b lst
  in
  __s3 input []
