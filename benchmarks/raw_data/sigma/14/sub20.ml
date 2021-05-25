(*
** PL::HW[01].Problem[01]
**
** Last Mod.: 2014-09-14 20:33
** Writ. by : CMS
*)

let rec sigma (a, b, f) = if a > b then 0 else f a + sigma (a+1, b, f)

