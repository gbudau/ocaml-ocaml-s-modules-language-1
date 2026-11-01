let () = 
 let rec print_cards f cards =
    match cards with
    | [] -> print_char '\n'
    | [head] -> print_string (f head); print_char '\n'
    | head::tail -> print_string (f head); print_string ", "; print_cards f tail
 in

 print_endline "Print all Spades";
 print_cards Card.toString Card.allSpades;
 print_newline ();

 print_endline "Print all Spades verbose";
 print_cards Card.toStringVerbose Card.allSpades;
 print_newline ();

 print_endline "Print all Hearts";
 print_cards Card.toString Card.allHearts;
 print_newline ();

 print_endline "Print all Hearts verbose";
 print_cards Card.toStringVerbose Card.allHearts;
 print_newline ();

 print_endline "Print all Diamonds";
 print_cards Card.toString Card.allDiamonds;
 print_newline ();

 print_endline "Print all Diamonds verbose";
 print_cards Card.toStringVerbose Card.allDiamonds;
 print_newline ();

 print_endline "Print all Clubs";
 print_cards Card.toString Card.allClubs;
 print_newline ();

 print_endline "Print all Clubs verbose";
 print_cards Card.toStringVerbose Card.allClubs;
 print_newline ();

 print_endline "Print all cards";
 print_cards Card.toString Card.all;
 print_newline ();

 print_endline "Print all cards verbose";
 print_cards Card.toStringVerbose Card.all;
 print_newline ();


 print_endline "Print all cards colors (verbose)";
 print_cards (fun t -> Card.Color.toStringVerbose (Card.getColor t)) Card.all;
 print_newline ();

 print_endline "Print all cards values (verbose)";
 print_cards (fun t -> Card.Value.toStringVerbose (Card.getValue t)) Card.all;
 print_newline ();


 print_endline "Compare Card(2, Spade) with Card(2, Heart)";
 print_endline (string_of_int (Card.compare (Card.newCard Card.Value.T2 Card.Color.Spade) (Card.newCard Card.Value.T2 Card.Color.Heart)));
 print_newline ();

 print_endline "Compare Card(3, Spade) with Card(2, Diamond)";
 print_endline (string_of_int (Card.compare (Card.newCard Card.Value.T3 Card.Color.Spade) (Card.newCard Card.Value.T2 Card.Color.Diamond)));
 print_newline ();

 print_endline "Compare Card(2, Spade) with Card(3, Diamond)";
 print_endline (string_of_int (Card.compare (Card.newCard Card.Value.T2 Card.Color.Spade) (Card.newCard Card.Value.T3 Card.Color.Diamond)));
 print_newline ();

 
 let rec print_card card =
    print_string (Card.toStringVerbose card); print_char '\n'
 in

 print_endline "Max Card(3, Spade) with Card(2, Heart)";
 print_card (Card.max (Card.newCard Card.Value.T3 Card.Color.Spade) (Card.newCard Card.Value.T2 Card.Color.Heart));
 print_newline ();

 print_endline "Max Card(2, Spade) with Card(3, Heart)";
 print_card (Card.max (Card.newCard Card.Value.T2 Card.Color.Spade) (Card.newCard Card.Value.T3 Card.Color.Heart));
 print_newline ();

 print_endline "Max Card(2, Heart) with Card(2, Spade)";
 print_card (Card.max (Card.newCard Card.Value.T2 Card.Color.Heart) (Card.newCard Card.Value.T2 Card.Color.Spade));

 print_endline "Min Card(3, Spade) with Card(2, Heart)";
 print_card (Card.min (Card.newCard Card.Value.T3 Card.Color.Spade) (Card.newCard Card.Value.T2 Card.Color.Heart));
 print_newline ();

 print_endline "Min Card(2, Spade) with Card(3, Heart)";
 print_card (Card.min (Card.newCard Card.Value.T2 Card.Color.Spade) (Card.newCard Card.Value.T3 Card.Color.Heart));
 print_newline ();

 print_endline "Min Card(2, Heart) with Card(2, Spade)";
 print_card (Card.min (Card.newCard Card.Value.T2 Card.Color.Heart) (Card.newCard Card.Value.T2 Card.Color.Spade));
 print_newline ();


 print_endline "Best of all cards";
 print_card (Card.best Card.all);
 print_newline ();

 print_endline "Best of empty cards list";
 try print_card (Card.best []) with Invalid_argument e -> print_endline e;
 print_newline ();


 print_endline "Card isOf Card(As, Diamond) Diamond";
 print_endline (string_of_bool (Card.isOf (Card.newCard Card.Value.As Card.Color.Diamond) Card.Color.Diamond));
 print_newline ();

 print_endline "Card isOf Card(As, Diamond) Club";
 print_endline (string_of_bool (Card.isOf (Card.newCard Card.Value.As Card.Color.Diamond) Card.Color.Club));
 print_newline ();


 print_endline "Card isSpade Card(As, Spade)";
 print_endline (string_of_bool (Card.isSpade (Card.newCard Card.Value.As Card.Color.Spade)));
 print_newline ();

 print_endline "Card isSpade Card(As, Diamond)";
 print_endline (string_of_bool (Card.isSpade (Card.newCard Card.Value.As Card.Color.Diamond)));
 print_newline ();


 print_endline "Card isHeart Card(As, Heart)";
 print_endline (string_of_bool (Card.isHeart (Card.newCard Card.Value.As Card.Color.Heart)));
 print_newline ();

 print_endline "Card isHeart Card(As, Diamond)";
 print_endline (string_of_bool (Card.isHeart (Card.newCard Card.Value.As Card.Color.Diamond)));
 print_newline ();


 print_endline "Card isDiamond Card(As, Diamond)";
 print_endline (string_of_bool (Card.isDiamond (Card.newCard Card.Value.As Card.Color.Diamond)));
 print_newline ();

 print_endline "Card isDiamond Card(As, Club)";
 print_endline (string_of_bool (Card.isDiamond (Card.newCard Card.Value.As Card.Color.Club)));
 print_newline ();


 print_endline "Card isClub Card(As, Club)";
 print_endline (string_of_bool (Card.isClub (Card.newCard Card.Value.As Card.Color.Club)));
 print_newline ();

 print_endline "Card isClub Card(As, Diamond)";
 print_endline (string_of_bool (Card.isClub (Card.newCard Card.Value.As Card.Color.Diamond)));
 print_newline ();

 ()
