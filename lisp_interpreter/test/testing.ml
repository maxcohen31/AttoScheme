(*
  This file contains some test cases for the AttoScheme interpreter.
  Abbate Emanuele - Winter 2026
*)

open Lisp_interpreter.Eval
open Lisp_interpreter.Parser

module Eval_test = struct 
  (* Arithmetic operations *)
  module Arithmetic = struct
    let op () = 
      Alcotest.(check string) "Addition" "3" (print_result (eval (parser "(+ 1 2)")));
      Alcotest.(check string) "Subtraction" "-10"  (print_result (eval (parser "(- 10 20)")));
      Alcotest.(check string) "Moltiplication" "8"  (print_result (eval (parser "(* 1 8)")));
      Alcotest.(check string) "Division with no remainder" "2"  (print_result (eval (parser "(/ 10 5)")));
      Alcotest.(check string) "Division with remainder" "1"  (print_result (eval (parser "(/ 10 6)")));
      Alcotest.(check string) "Mod" "0"  (print_result (eval (parser "(% 100 10)"))); 
      Alcotest.(check string) "Nested expression" "28"  (print_result (eval (parser "(* (+ 2 5) (- 7 (/ 21 7)))" ))); 
  end
  (* Basic expressions *)
  module Basix_exp = struct
    let op () = 
      Alcotest.(check string) "Positive integer" "89" (print_result (eval (parser "89")));
      Alcotest.(check string) "Negative integer" "-19" (print_result (eval (parser "-19")));
      Alcotest.(check string) "True" "#t" (print_result (eval (parser "#t")));
      Alcotest.(check string) "False" "#f" (print_result (eval (parser "#f")));
  end
  (* String handling *)
  module Strings = struct
    let op () = 
      Alcotest.(check string) "Return string" "\"Hello, World!\"" (print_result (eval (parser "\"Hello, World!\"")));
      Alcotest.(check string) "Empty string" "\"\"" (print_result (eval (parser "\"\"")));
      Alcotest.(check string) "Escaping characters" "\"She said, \\\"Sells she seashells on the seashore?\\\"\"" 
        (print_result (eval (parser "\"She said, \\\"Sells she seashells on the seashore?\\\"\"")));
  end
  (* If statements *)
  module Conditionals = struct
    let op () = 
      Alcotest.(check string) "If #t" "10" (print_result (eval (parser "(if #t 10 11)")));
      Alcotest.(check string) "If #f" "11" (print_result (eval (parser "(if #f 10 11)")));
      Alcotest.(check string) "if gt" "10" (print_result (eval (parser "(if (> 1 2) (% 4 2) (* 5 2))")));
      Alcotest.(check string) "if lt" "0" (print_result (eval (parser "(if (< 1 2) (% 4 2) (* 5 2))")));
      Alcotest.(check string) "if not equal" "99" (print_result (eval (parser "(if (\\= 1 2) 99 100)")));
      Alcotest.(check string) "if gte" "0" (print_result (eval (parser "(if (<= 20 25) 0 1)")));
      Alcotest.(check string) "if lte" "1" (print_result (eval (parser "(if (>= 20 25) 0 1)")));
  end
  (* List manipulations *)
  module List_op = struct
    let op () = 
      Alcotest.(check string) "quoted list" "(1 2 3)" (print_result (eval (parser ("'(1 2 3)"))));
      Alcotest.(check string) "list with different types" "((\"mhanz\" 2) 3)" (print_result (eval (parser "'((\"mhanz\" 2) 3)"))); 
      Alcotest.(check string) "CAR operation" "a" (print_result (eval (parser "(car '(a b c))"))); 
      Alcotest.(check string) "CONS CAR CDR operations" "(a e f)" 
        (print_result (eval (parser "(cons (car '(a b c)) (cdr '(d e f)))"))); 
      Alcotest.(check string) "CONS operation" "(3 2 1)" (print_result (eval (parser "(cons 3 (cons 2 (cons 1 '())))"))); 
      Alcotest.(check string) "CDR operation" "(b c)" (print_result (eval (parser "(cdr '(a b c))"))); 
  end
  (* Connectives operations *)
  module LogicalConnectives_op = struct
    let op () = 
      Alcotest.(check string) "OR True" "#t" (print_result (eval (parser "(or #t #f)")));
      Alcotest.(check string) "OR False" "#f" (print_result (eval (parser "(or #f #f)")));
      Alcotest.(check string) "AND False" "#f" (print_result (eval (parser "(and #f #t)"))); 
      Alcotest.(check string) "AND True" "#t" (print_result (eval (parser "(and #t #t)"))); 
      Alcotest.(check string) "not #t" "#f" (print_result (eval (parser "(not #t)")));
  end
  (* Bitwise operations *)
  module Bitwise_op = struct
    let op () = 
      Alcotest.(check string) "bitwise AND" "1" (print_result (eval (parser "(bitand 3 5)")));
      Alcotest.(check string) "bitwise Or" "7" (print_result (eval (parser "(bitor 3 5)")));
      Alcotest.(check string) "bitwise Xor" "6" (print_result (eval (parser "(bitxor 3 5)")));
      Alcotest.(check string) "bitwise Not" "-4" (print_result (eval (parser "(bitwise-not 3)")));
  end
  (* Comparison tests *)
  module Comparison_op = struct
    let op () = 
      Alcotest.(check string) "Equals to" "#t" (print_result (eval (parser "(= 11 11)"))) ;
      Alcotest.(check string) "greater than" "#t" (print_result (eval (parser "(> 1 0)"))) ;
      Alcotest.(check string) "less than" "#f" (print_result (eval (parser "(< 6 4)"))) ;
      Alcotest.(check string) "greater equal than" "#f" (print_result (eval (parser "(>= 6 10)"))) ; 
      Alcotest.(check string) "less equal than" "#t" (print_result (eval (parser "(<= 6 6)"))) ; 
  end
end

(* Main driver *)
let () = 
  Alcotest.run "All Tests" [
  ("Eval basic", [
    Alcotest.test_case "Arithmetic" `Quick Eval_test.Arithmetic.op;
    Alcotest.test_case "Basic Expressions" `Quick Eval_test.Basix_exp.op;
    Alcotest.test_case "String tests" `Quick Eval_test.Strings.op;
    Alcotest.test_case "Conditionals tests" `Quick Eval_test.Conditionals.op;
    Alcotest.test_case "List tests" `Quick Eval_test.List_op.op;
    Alcotest.test_case "Logical Connectives tests" `Quick Eval_test.LogicalConnectives_op.op;
    Alcotest.test_case "Comparisons" `Quick Eval_test.Comparison_op.op;
    Alcotest.test_case "Bitwise operations" `Quick Eval_test.Bitwise_op.op;
  ]); 
  ]
