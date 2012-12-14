open Ast

(* file: main.ml *)
let main () =
  try
    let source = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel source in
    let result = Parser.program Scanner.token lexbuf in
        print_string("done parsing... now generate the code"); flush stdout;
  with Scanner.Eof -> exit 0
      
(*let _ = Printexc.print main ()*)
let _ = main()
