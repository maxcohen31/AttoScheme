(*
  An implementation of a simple Scheme interpreter written in Ocaml. 

  Author: Abbate Emanuele - Winter 2026
*)

open Base
open Stdio

(******************** Data *********************)
type token = 
  | Number of int 
  | Symbol of string
  | Boolean of bool
  | String of string
  | LPar 
  | RPar
  | Quote

type sexpr = 
  | Atom of token
  | List of sexpr list

(********************* Utility functions ********************)

let is_delimiter: char -> bool = function 
  | ' ' | '(' | ')' | '\n' | '\t' -> true 
  | _ -> false  ;;

let is_valid_symbol: char -> bool = function
  | 'a'..'z' | 'A'..'Z'   
  | '>' | '<' | '=' | '%'
  | '*' | '-' | '+' | '/' -> true
  | _ -> false

let is_digit: char -> bool = function
  | '0' .. '9' -> true 
  | _ -> false ;;

exception LexerError of string 

(* Takes a string as parameter and match it *)
let match_string text = 
  let regex = Re.Perl.re "(\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\")" |> Re.compile in 
    match Re.exec_opt regex text with 
    | Some g -> Re.Group.get g 0 
    | None -> raise (LexerError "Unterminated string literal") ;;

(********************* Lexing process **********************)

(* Scan a string and returns a pair int * string - (index, string)*)
let get_token (str:string) (i: int) = 
  (* We use a buffer to concatenate characters found along the scanning subroutine *)
  let buf = Buffer.create 64 in 
    let rec aux idx =
      (* Base case *)
      if idx >= String.length str then (idx, Buffer.contents buf) 
      else 
        let ch = String.get str idx in 
          match ch with
          (* Hit a delimiter *)
          | c when is_delimiter c -> (idx, Buffer.contents buf) 
          | c -> Buffer.add_char buf c; aux (idx+1)
  in aux i ;;

let extract_string (str:string): int * string = 
  let matched = match_string str in 
    let stripped = String.sub matched ~pos:1 ~len:(String.length matched-2) in 
      (String.length matched, stripped) ;;

(* Takes a string as parameter and returns a tokens list *)
let lexer (str:string): token list = 
  let rec tokenizer idx acc = 
    (* Base case *)
    if idx >= String.length str then List.rev acc
    else 
      let ch = String.get str idx in 
        match ch with 
        | ' ' | '\t' | '\n' -> tokenizer (idx+1) acc
        | '(' -> tokenizer (idx+1) (LPar :: acc)
        | ')' -> tokenizer (idx+1) (RPar :: acc)
        | '\'' -> tokenizer (idx+1) (Quote :: acc)
        | '"' -> 
            let substr = String.sub str ~pos:idx ~len:(String.length str - idx) in 
            let new_idx, s = extract_string substr in tokenizer (idx + new_idx) (String s :: acc);
        | _ -> 
            let i, tok = get_token str idx in 
            match tok with
            | "#t" -> tokenizer i (Boolean true :: acc)
            | "#f" -> tokenizer i (Boolean false :: acc)
            | ">=" -> tokenizer i (Symbol ">=" :: acc)
            | "<=" -> tokenizer i (Symbol "<=" :: acc)
            | "\\=" -> tokenizer i (Symbol "\\=" :: acc)
            | _ -> 
              (* Check either the token is a positive integer or a negative integer *)
              let is_number = 
                match String.get tok 0 with
                | c when is_digit c -> true
                | '-' when String.length tok > 1 && is_digit (String.get tok 1) -> true
                | _ -> false
              in 
              if is_number then
                tokenizer i (Number (Int.of_string tok) :: acc)
              else if is_valid_symbol (String.get tok 0) then
                tokenizer i (Symbol tok :: acc)
              else
                raise (LexerError("Invalid character: " ^ tok))
  in
  tokenizer 0 [] ;;

(* Take a token list and prints it on stdout *)
let print_token_list (tok_lst: token list): unit = 
  List.iter ~f:(fun t -> 
    match t with 
    | Symbol s -> printf "Symbol (%s) " s 
    | Number n -> printf "Number (%s) " (Int.to_string n)
    | LPar -> printf "("
    | RPar -> printf ")"
    | Quote -> printf "'"
    | String str -> printf "String (%s)" str
    | Boolean b -> match b with
                   | false -> printf "Boolean (F) "
                   | _ -> printf "Boolean (T) "
  )
  tok_lst;
  printf "\n" ;;

(********************* Parsing process ********************)

exception ParseError of string 
exception UnexpectedParenthesis of string

let parser (sexp: string): sexpr  = 
  let tokens = ref (lexer sexp) in 
    let peek () = 
      match !tokens with 
      | [] -> raise (ParseError "Peek failed")
      | x :: _ -> x 
    in 
    let consume_token () = 
      match !tokens with 
      | [] -> raise (ParseError "consume_atom: empty list")
      | _ :: tl -> tokens := tl
    in
    let rec parse_sexp () = 
        match peek () with 
       | LPar -> parse_lst ()
       | Number _ -> parse_atom () 
       | Symbol _ -> parse_atom ()
       | Boolean _ -> parse_atom () 
       | String _ -> parse_atom ()
       | RPar -> 
           raise (UnexpectedParenthesis "Unexpected )")
       | Quote -> 
           consume_token (); 
           let e = parse_sexp () in 
           List [Atom (Symbol "quote"); e]
    and
    parse_lst() =
      (* Consumes '(' *)
      consume_token ();
      let rec aux acc = 
        match peek () with 
        | RPar -> 
            (* Consumes ')' *)
            consume_token ();
            List (List.rev acc)
        | _ -> 
            let el = parse_sexp () in 
            aux (el :: acc)
      in aux [] 
    and
    parse_atom () = 
      let tok = peek () in 
      consume_token ();
      Atom tok
    in 
    let result_sexp = parse_sexp () in 
      match !tokens with 
      | [] -> result_sexp 
      | _ -> raise (ParseError "Invalid value") ;;

exception PrintError of string 
(* Take a S-expression and prints on stdout its Scheme representation
   Mostly used for debugging *)
let rec pp_sexp = function 
  | Atom (Number n) -> Int.to_string n 
  | Atom (Boolean b) -> 
      (match b with
      | true -> "#t"
      | false -> "#f")
  | Atom (Symbol s) -> s
  | Atom (String str) -> "\"" ^ str ^ "\""
  | List [Atom (Symbol "'"); List []] -> "'()" 
  | List [] -> "()"
  | List ls -> "(" ^ String.concat ~sep:" " (List.map ls ~f:pp_sexp) ^ ")" 
  | _ -> raise (PrintError "Printing failed") ;;
 
