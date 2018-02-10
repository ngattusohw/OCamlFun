(* Nick Gattuso
I pledge my honor that I have abided by the Stevens Honor System *)


(* add : int -> int -> int *)
let add x y = x+y;;

type calcExp =
	| Const of int
	| Add of (calcExp*calcExp)
	| Sub of (calcExp*calcExp) | Mult of (calcExp*calcExp)
	| Div of (calcExp*calcExp);;

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  EXERCISE 1 !!!!!!!!!!!!!!!!!!!!!!!!! *)

(* val seven : 'a -> int = <fun> *)
let seven i = 7;;

(* val sign : int -> int = <fun> *)
let sign i = if i>0 then 1 else if i<0 then -1 else 0;;

(* val absolute : int -> int = <fun> *)
let absolute i = if i<0 then i * (-1) else i;;

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

(* val swap : 'a * 'b -> 'b * 'a = <fun> *)
let swap (x,y) = (y,x);;

(* val app : ('a -> 'b) -> 'a -> 'b = <fun> *)
let app f i = f i;;

(* val twice : ('a -> 'a) -> 'a -> 'a = <fun> *)
let twice f i = f (f i);;

(* val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)
let compose f g i = f ( g i );;


(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Exercise 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

(* val remAdjDups : 'a list -> 'a list = <fun> *)
let rec remAdjDups = function
	| [] -> []
	| [x] -> [x]
	| x::y::xs -> if x = y then remAdjDups (y::xs) else x :: remAdjDups (y::xs);;

(* val belongsTo_ext : 'a -> 'a list -> bool = <fun> *)
let rec belongsTo_ext elem l =
	match l with
	| [] -> false
	| x::xs -> if x = elem then true else belongsTo_ext elem xs;;

(* val union_char : 'a -> ('a -> bool) -> ('a -> bool) -> bool = <fun> *)
let union_char x f1 f2 = (f1 x) || (f2 x);;

(* val union_ext : 'a list -> 'a list -> 'a list = <fun> *)
let union_ext l1 l2 = remAdjDups (List.sort compare (l1@l2));;

(* val intersection_ext : 'a list -> 'a list -> 'a list = <fun> *)
let rec intersection_ext l1 l2 = 
	let f = 
		match l1 with
		| [] -> []
		| x::xs -> if (belongsTo_ext x l2) = true then x :: (intersection_ext xs l2)
				else intersection_ext xs l2
	in remAdjDups (List.sort compare f);;

(* val intersection_char : 'a -> ('a -> bool) -> ('a -> bool) -> bool = <fun> *)
let intersection_char x f1 f2 = (f1 x) && (f2 x);;

(* val belongsTo_char : 'a -> ('a -> bool) -> bool = <fun> *)
let belongsTo_char x f = if f x then true else false;;

(* val sublists : 'a list -> 'a list list = <fun> *)
let rec sublists = function
	| [] -> [[]]
	| x::xs -> let ls = sublists xs in
               List.map (fun l -> x::l) ls @ ls;;

(********************** EXERCISE 3 ****************************)

(* val mapC : (int -> int) -> calcExp -> calcExp = <fun> *)
let rec mapC f elem =
	match elem with
	| Const(i) -> Const(f i)
	| Add(i,j) -> Add(mapC f i, mapC f j)
	| Sub(i,j) -> Sub(mapC f i, mapC f j)
	| Mult(i,j) -> Mult(mapC f i, mapC f j)
	| Div(i,j) -> Div(mapC f i, mapC f j);;


(* val foldC : (int -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> calcExp -> 'a = <fun> *)
let rec foldC c a s m d elem = 
	match elem with
	| Const(i) -> c i
	| Add(i,j) -> a (foldC c a s m d i) (foldC c a s m d j)
	| Sub(i,j) -> s (foldC c a s m d i) (foldC c a s m d j)
	| Mult(i,j) -> m (foldC c a s m d i) (foldC c a s m d j)
	| Div(i,j) -> d (foldC c a s m d i) (foldC c a s m d j);;

(* val numAdd : calcExp -> int = <fun> *)
let numAdd elem = foldC (fun i -> 0) (fun x y -> x + y + 1) (+) (+) (+) elem;;

(* val replaceAddWithMult : calcExp -> calcExp = <fun> *)
let replaceAddWithMult elem = foldC (fun i -> Const(i)) (fun x y -> Mult(x,y)) (fun x y -> Sub(x,y))
								(fun x y -> Mult(x,y)) (fun x y -> Div(x,y)) elem;;

(* val evalC : calcExp -> int = <fun> *)
let rec evalC elem = 
	match elem with
	| Const(i) -> i
	| Add(i,j) -> (evalC i) + (evalC j)
	| Sub(i,j) -> (evalC i) - (evalC j)
	| Mult(i,j) -> (evalC i) * (evalC j)
	| Div(i,j) -> (evalC i) / (evalC j);;

(* val evalCf : calcExp -> int = <fun> *)
let rec evalCf elem = foldC (fun i -> i) (+) (-) ( * ) (/) elem;;



(* !!!!!!!!!!!!!!!!!!!!!!!!!!! EXERCISE 4!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

(* let f xs =
	let g = fun x r -> if x mod 2 = 0 then (+) r 1 else r
	in List.fold_right g xs 0 
1 The above function will go through a list, and with a function g, check to see
 if each number in the list is even. If it is, then it will increment a counter, and
 it uses fold to go through the list.

 With the example  'f [1;3;4;5];;', the return value is 1, because there is only one even number
 For 'f [2;4;6;9];;' , the return value is 3, because there is only 3 even numbers
 Finally, for 'f [1;3;5];;', the return value is 0, because there are no even numbers within the list. *)

(* val append : 'a list -> 'a list -> 'a list = <fun> *)
let append xs =
	let g = fun x h -> x::h
in List.fold_right g xs;;


