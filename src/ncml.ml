

(* file: main.ml *)
let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = (* Parser.program *) Parser.program Scanner.token lexbuf in
        print_newline(); flush stdout (* should write function to pretty print the generated ast *)
    done
  with Scanner.Eof -> exit 0
      
let _ = Printexc.print main ()
