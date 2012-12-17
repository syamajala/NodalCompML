open Ast
open Generator

(* file: main.ml *)
(* let main () =
  try
    let source = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel source in
    let result = Parser.program Scanner.token lexbuf in
    (*slip the resulting string representation of the ast into a python code file that will execute it using
      the python interpreter*)
    let output = "from ast import *\nfrom DEBUG_AST_PRETTY_PRINTER import *\naststr=\"" ^ (string_of_program result) ^ "\"\nast=eval(aststr)\nast=fix_missing_locations(ast)\nprint dump(ast)\nobj=compile(ast,\"\",\"exec\")\nexec obj" in
    let dest = open_out "IR.py" in
    output_string dest output;
    close_out dest;
    let compile_result = Sys.command "python IR.py" in
    match compile_result with
	0 -> "success!"
      | _ -> "error python ast compilation.";
  with Scanner.Eof -> "end of file input." *)
      
let main_gen () =
  try
    let source = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel source in
    let result = Parser.program Scanner.token lexbuf in
    let python_source_code = (str_of_program result) in
    let dest = open_out "IR.py" in
    output_string dest python_source_code;
    close_out dest;
    let compile_result = Sys.command "python IR.py" in
    match compile_result with
	0 -> "success!"
      | _ -> "error in python code compilation";
  with Scanner.Eof -> "end of file input."

let _ = main_gen()

