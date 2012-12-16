open Ast

(* file: main.ml *)
let main () =
  try
    let source = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel source in
    let result = Parser.program Scanner.token lexbuf in
    print_endline (string_of_program result)
  with Scanner.Eof -> print_endline("end of file input.")
      
let _ = main()
