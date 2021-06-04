let rec __s1 (__s2 : 'b -> 'a -> 'a) (__s3 : 'b list) (__s4 : int) =
  match __s3 with
  | [] -> __s4
  | __s11 :: __s12 -> __s2 __s11 (__s1 __s2 __s12 __s4)


let rec max (lst : int list) : int =
  match lst with
  | [] -> raise Failure "Exception(Template)"
  | __s7 :: __s8 ->
      __s1
        (fun (__s9 : int) (__s10 : int) -> if __s9 > __s10 then __s9 else __s10)
        __s8 __s7
