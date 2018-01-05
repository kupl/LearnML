(*
 * Brief      : HW2, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 23, 2014
 *)

(* Exercise 1 : CheckMetroMap *)
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro: metro -> bool = fun m ->
  let rec checkMetroSub: name list -> metro -> bool = fun nl m ->
    match m with
    | STATION name             -> (List.mem name nl)
    | AREA (name, metro)       -> (checkMetroSub (name::nl) metro)
    | CONNECT (metro1, metro2) -> (checkMetroSub nl metro1) && (checkMetroSub nl metro2)
  in
  (checkMetroSub [] m)