open OUnit2
open Ast
open Ds
open Interp

(* A few test cases *)
let tests = [
  "int"  >:: (fun _ -> assert_equal (NumVal 22) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (NumVal 22) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (NumVal 22) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (NumVal 22) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (NumVal 22) (interp "let x = 0 in let x = 22 in x"));
  "abs"  >:: (fun _ -> assert_equal (NumVal 1) (interp "abs((-1))"));
  "hd"  >:: (fun _ -> assert_equal (NumVal 1) (interp "hd(cons(1,emptylist))"));
  "null"  >:: (fun _ -> assert_equal (BoolVal false) (interp "null?(cons(1,emptylist))"));
]

let _ = run_test_tt_main ("suite" >::: tests)
