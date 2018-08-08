open Syntax
open Eval
open Infer
       
let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  match  (infer_cmd tyenv cmd) with
  |exception InferErr -> read_eval_print env tyenv
  |(ty,newtyenv) ->
  match (eval_command env cmd) with
  |exception _ -> (read_eval_print env tyenv)
  |(id, newenv, v) ->
  (Printf.printf "%s = " id;
   print_value v;
   print_newline ();
   read_eval_print newenv newtyenv)

let initial_env = empty_env
    
let _ = read_eval_print initial_env initial_env
