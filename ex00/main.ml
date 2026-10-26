
let () = 
 let rec print_cards f cards =
    match cards with
    | [] -> print_char '\n'
    | [head] -> print_string (f head); print_char '\n'
    | head::tail -> print_string (f head); print_string ", "; print_cards f tail
 in
 print_cards Color.toString Color.all;
 print_cards Color.toStringVerbose Color.all;
 ()
