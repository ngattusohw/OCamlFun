(* add : int -> int -> int *)
let add x y = x+y;;

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  EXERCISE 1 !!!!!!!!!!!!!!!!!!!!!!!!! *)

(* val seven : 'a -> int = <fun> *)
let seven i = 7;;

(* val sign : int -> int = <fun> *)
let sign i = if i>0 then 1 else if i<0 then -1 else 0;;

(* val absolute : int -> int = <fun> *)
let absolute i = if i<0 then i * ~-1 else i;;

(* val andp : bool -> bool -> bool = <fun> *)
let andp i j = if i && j then true else false;;

(* val orp : bool -> bool -> bool = <fun> *)
let orp i j = if i || j then true else false;;

(* val notp : bool -> bool = <fun> *)
let notp i = if not i then true else false;;

let xorp i j = if (i || j) && (not (i && j)) then true else false;;

(* val dividesBy : int -> int -> bool = <fun> *)
let dividesBy i j = if j = 0 then false
						else if i mod j = 0
							then true
							else false;;
(* val is_singleton : 'a list -> bool = <fun> *)
let is_singleton l = 
	match l with
	| [] -> false
	| [x] -> true
	| _ -> false;;

(* TESTS *)
print_string "Seven tests::\n";;

let seven1 = seven 2;;
print_int seven1;;

print_string "\nSign tests::\n";;

let sign1 = sign ~-200;;
print_int sign1;;

print_string "\n Divides by Test\n";;

(* let divides = dividesBy 0 0;;
if divides = true
	then print_string "\n I Was true\n"
	else print_string "\n I was false \n" *)

(* print_string "\nAbs tests::\n"; *)

(* let abs1 = absolute ~-20;;
print_int abs1;; *)

print_string "\nConditional Test\n";;


let conditonal_test1 = orp false false;;
if conditonal_test1 = true
	then print_string "I was true\n"
	else print_string "I was false\n"


(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Exercise 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

