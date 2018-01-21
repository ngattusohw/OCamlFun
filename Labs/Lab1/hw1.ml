(* add : int -> int -> int *)
let add x y = x+y;;

(* EXERCISE 1!!!!!!!! *)

(* val seven : 'a -> int = <fun> *)
let seven i = 7;;

(* val sign : int -> int = <fun> *)
let sign i = if i>0 then 1 else if i<0 then -1 else 0;;

(* val absolute : int -> int = <fun> *)
let absolute i = if i<0 then i * ~-1 else i;;

(* val andp : bool -> bool -> bool = <fun> *)
let andp i j = if i && j then true else false;;

let dividesBy i j = if i mod j = 0 then true else false;;

(* TESTS *)
print_string "Seven tests::\n";;

let seven1 = seven 2;;
print_int seven1;;

print_string "Sign tests::\n";;

let sign1 = sign ~-200;;
print_int sign1;;

print_string "Abs tests::\n";

let abs1 = absolute ~-20;;
print_int abs1;;

print_string "\n";;