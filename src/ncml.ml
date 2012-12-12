open Ast

(* file: main.ml *)
let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    (*while true do*)
      let result = Parser.program Scanner.token lexbuf in
        print_string("done parsing. "); flush stdout; (* should pretty-print the ast *)
    (*done*)
  with Scanner.Eof -> exit 0
      
(*let _ = Printexc.print main ()*)
let _ = main()
