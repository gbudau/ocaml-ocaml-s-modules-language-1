let () = 
 let rec print_cards f cards =
    match cards with
    | [] -> print_char '\n'
    | [head] -> print_string (f head); print_char '\n'
    | head::tail -> print_string (f head); print_string ", "; print_cards f tail
 in
 print_cards Value.toString Value.all;
 print_cards Value.toStringVerbose Value.all;
 print_cards (fun t -> string_of_int (Value.toInt t)) Value.all;
 print_cards (fun t -> try Value.toString (Value.next t) with Invalid_argument e -> e) Value.all;
 print_cards (fun t -> try Value.toString (Value.previous t) with Invalid_argument e -> e) Value.all;
 ()
