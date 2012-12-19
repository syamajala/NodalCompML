let bool_list = [true;false;false;true];;

let tab_level = "\t\t";;

let str_of_bool = function
  | true -> "\t True"
  | false -> "\t\t False"

let tabify str = function
  | tabs -> tabs ^ str

let main () =
(*  List.map print_endline (List.map str_of_bool bool_list)*)
  (List.map print_endline (List.map (tabify tab_level) (List.map str_of_bool bool_list)))


let _ = main()
