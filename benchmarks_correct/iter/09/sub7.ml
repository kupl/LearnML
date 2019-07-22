exception Error of string;;
let rec iter( n, f ) = 
  let compose f g = 
	function x -> f( g( x ) ) in
  if( n < 0 ) then raise (Error "negative value!")
  else if( n == 0 ) then function x -> x
  else compose f ( iter( n - 1, f ) );;

