(*
  This file contains the code of the repl function.
  Abbate Emanuele - Winter 2026
*)


open Stdio
open Lisp_interpreter
open Eval
open Parser

let banner () = 
  let banner_strings = 
    [
      "----------------------------------";
      "|     ΛTTOSCHEME INTERPRETER     |"; 
      "----------------------------------";
    ] 
  in 
  List.iter (fun s -> printf "%s\n" s) banner_strings ;; 

let repl () =  
  while true do
    printf ">> ";
    flush stdout;
    let line = In_channel.input_line In_channel.stdin in 
      match line with
      | None -> exit 0
      | Some l -> 
          let sexp = eval (parser l) in 
            pp_res sexp 
  done;;

let () = 
  banner();
  flush stdout;
  repl();
