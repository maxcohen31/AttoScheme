(*
  This file contains the code for the evaluation function. 
  Emanuele Abbate - Winter 2026
*)

open Base 
open Stdio
open Parser


(*********** Runtime errors ***********)
exception RuntimeError of string 
exception AdditionError of string
exception SubtractionError of string
exception MultiplicationError of string
exception DivisionError of string
exception ModuloError of string
exception DivisionByZero of string
exception ComparisonError of string
exception LogicalError of string
exception AndError of string
exception OrError of string
exception XorError of string
exception BitwiseNotError of string


(*********** Eval function ************)
let rec eval sexpr = 
  match sexpr with
  (* Atom evaluates in itself *)
  | Atom (Boolean _) as b -> b
  | Atom (Number _) as n -> n
  | Atom (String _) as s -> s 
  (* TODO: To change if an environment is implemented *)
  | Atom (Symbol _) as sym -> sym 

  (* ***** Empty list ***** *)
  | List [] -> List []

  (* ***** Quote ***** *)
  | List [Atom (Symbol "'"); e] | List [Atom (Symbol "quote"); e] -> e

  (* ***** IF statement ****** *)
  | List [Atom (Symbol "if"); exp; if_then; if_else] ->
      let e = eval exp in 
      (match e with
       | Atom (Boolean false)-> eval if_else
       | _ -> eval if_then)
  
  (* ***** Logical connectives ***** *)
  | List [Atom (Symbol "and"); e1; e2] ->
    (match eval e1 with
     | Atom (Boolean false) -> Atom (Boolean false)
     | _ -> eval e2)

  | List [Atom (Symbol "or"); e1; e2] ->
      (match eval e1 with
      | Atom (Boolean false) -> eval e2
      | v -> v)

  (* ***** Apply function ***** *)
  | List (f :: args) -> 
      let fn = eval f in 
       let rest = List.map args ~f:eval in 
         apply fn rest 
  | _ -> raise (RuntimeError "Invalid expression") 
  
and
  apply func args = 
    match func, args with
    (* ***** Primitive operations: +, -, *, / ***** *)
    | Atom (Symbol "+"), [x; y] -> 
        (match x, y with
         | Atom (Number x), Atom (Number y) -> 
             Atom (Number (x + y))
         | _ -> raise (AdditionError "Types must be Int"))
    | Atom (Symbol "-"), [x; y] -> 
        (match x, y with
         | Atom (Number x), Atom (Number y) -> 
             Atom (Number (x - y))
         | _ -> raise (SubtractionError "Types must be Int"))
    | Atom (Symbol "*"), [x; y] -> 
        (match x, y with
         | Atom (Number x), Atom (Number y) -> 
             Atom (Number (x * y))
         | _ -> raise (MultiplicationError "Types must be Int"))
    | Atom (Symbol "%"), [x; y] -> 
        (match x, y with
         | Atom (Number x), Atom (Number y) -> 
             Atom (Number (x % y))
         | _ -> raise (ModuloError "Types must be Int"))
    | Atom (Symbol "/"), [x; y] -> 
        (match x, y with
         | Atom (Number x), Atom (Number y) -> 
             if y = 0 then raise (DivisionByZero "[ERROR]: Cannot divide by zero") 
             else Atom (Number (x / y))
         | _ -> raise (DivisionError "Types must be Int"))

    (* ***** Comparison operations ***** *)
    | Atom (Symbol "<"), [x; y] ->
        (match x, y with
         | Atom (Number x), Atom (Number y) -> Atom (Boolean (x < y))
         | _ -> raise (ComparisonError "Operands for '<' must be numbers"))
    | Atom (Symbol "<="), [x; y] ->
        (match x, y with
         | Atom (Number x), Atom (Number y) -> Atom (Boolean (x <= y))
         | _ -> raise (ComparisonError "Operands for '<=' must be numbers"))
    | Atom (Symbol ">"), [x; y] ->
        (match x, y with
         | Atom (Number x), Atom (Number y) -> Atom (Boolean (x > y))
         | _ -> raise (ComparisonError "Operands for '>' must be numbers"))
    | Atom (Symbol ">="), [x; y] ->
        (match x, y with
         | Atom (Number x), Atom (Number y) -> Atom (Boolean (x >= y))
         | _ -> raise (ComparisonError "Operands for '>=' must be numbers"))
    | Atom (Symbol "="), [x; y] ->
        (match x, y with
         | Atom (Number x), Atom (Number y) -> Atom (Boolean (x = y))
         | _ -> raise (ComparisonError "Operands for '=' must be numbers"))
    | Atom (Symbol "\\="), [x; y] ->
        (match x, y with
         | Atom (Number x), Atom (Number y) -> Atom (Boolean (x <> y))
         | _ -> raise (ComparisonError "Operands for '\\=' must be numbers"))

    (* ***** Logical operations ***** *)
    | Atom (Symbol "and"), [x; y] -> 
        (match x, y with
         | Atom (Boolean n1), Atom (Boolean n2) -> 
             Atom (Boolean (n1 && n2))
         | _ -> raise (AndError "AND invalid types"))
    | Atom (Symbol "or"), [x; y] -> 
        (match x, y with
         | Atom (Boolean n1), Atom (Boolean n2) -> 
             Atom (Boolean (n1 || n2))
         | _ -> raise (OrError "OR invalid types"))
    | Atom (Symbol "not"), [x] ->
        (match x with
         | Atom (Boolean b) -> Atom (Boolean (not b))
         | _ -> raise (LogicalError "Operand for 'not' must be boolean"))

    (* ***** Bitwise operations ***** *) 
    | Atom (Symbol "bit-and"), [x; y] -> 
        (match x, y with
         | Atom (Number n1), Atom (Number n2) -> 
             Atom (Number (n1 land n2))
         | _ -> raise (AndError "AND invalid types"))
    | Atom (Symbol "bit-or"), [x; y] -> 
        (match x, y with
         | Atom (Number n1), Atom (Number n2) -> 
             Atom (Number (n1 lor n2))
         | _ -> raise (OrError "OR invalid types"))
    | Atom (Symbol "bit-xor"), [x; y] -> 
        (match x, y with
         | Atom (Number n1), Atom (Number n2) -> 
             Atom (Number (n1 lxor n2))
         | _ -> raise (XorError "XOR invalid types"))
    | Atom (Symbol "bitwise-not"), [x] -> 
        (match x with
         | Atom (Number n1) -> 
             Atom (Number (lnot n1))
         | _ -> raise (BitwiseNotError "Bitwise NOT invalid types"))

    (* ***** CONS, CAR and CDR operations ***** *)
    | Atom (Symbol "cons"), [x; List ls] -> List (x :: ls)
    | Atom (Symbol "cons"), [_; _] -> 
        raise (RuntimeError "[ERROR]: cons second argument must be a list")
    | Atom (Symbol "car"), [List []] -> 
        raise (RuntimeError "[ERROR]: car on empty list")
    | Atom (Symbol "car"), [List (hd :: _)] -> hd
    | Atom (Symbol "cdr"), [List (_ :: tl)] -> List tl
    | Atom (Symbol "cdr"), [List []] -> 
        raise (RuntimeError "[ERROR]: cdr on empty list")
    | _ -> raise (RuntimeError "[ERROR]: Apply Runtime error")

(* Take an S-expression and converts it to string type *)
let rec print_result: sexpr -> string = function
  | Atom (Number x) -> Int.to_string x
  | Atom (Boolean b) -> 
      (match b with
       | true -> "#t"
       | false -> "#f")
  | Atom (Symbol sym) -> sym
  | Atom (String s) -> "\"" ^ s ^ "\""
  | List [] -> "()"
  | List [Atom (Symbol "quote"); exp] -> "'" ^ print_result exp
  | List ls -> "(" ^ String.concat ~sep:" " (List.map ls ~f:print_result) ^ ")"
  | _ -> raise (PrintError "Error while printing S-expression")

(* Wrapper for print_result function *)
let pp_res (s: sexpr): unit = 
    printf "%s\n" (print_result s) ;;

