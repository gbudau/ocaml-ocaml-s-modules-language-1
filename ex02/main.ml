let () = 
  let rec print_cards f = function
    | [] -> print_newline ()
    | [head] -> print_endline (f head)
    | head :: tail ->
        print_string (f head ^ ", ");
        print_cards f tail
  in

  let print_card card =
    print_endline (Card.toStringVerbose card)
  in

  let run_test name f cards =
    print_endline ("--- " ^ name ^ " ---");
    print_cards f cards;
    print_newline ()
  in

  run_test "All Spades" Card.toString Card.allSpades;
  run_test "All Spades verbose" Card.toStringVerbose Card.allSpades;
  run_test "All Hearts" Card.toString Card.allHearts;
  run_test "All Diamonds verbose" Card.toStringVerbose Card.allDiamonds;
  run_test "All Clubs" Card.toString Card.allClubs;
  run_test "All cards" Card.toString Card.all;

  print_endline "--- Get Value and Color ---";
  print_cards (fun t -> Card.Color.toStringVerbose (Card.getColor t)) Card.allSpades;
  print_cards (fun t -> Card.Value.toStringVerbose (Card.getValue t)) Card.allSpades;
  print_newline ();

  let c2S = Card.newCard Card.Value.T2 Card.Color.Spade in
  let c2H = Card.newCard Card.Value.T2 Card.Color.Heart in
  let c3D = Card.newCard Card.Value.T3 Card.Color.Diamond in
  let cAD = Card.newCard Card.Value.As Card.Color.Diamond in

  let test_compare c1 c2 =
    print_endline (string_of_int (Card.compare c1 c2))
  in

  print_endline "--- Comparisons ---";
  print_string "Compare Card(2, Spade) with Card(2, Heart): "; test_compare c2S c2H;
  print_string "Compare Card(3, Diamond) with Card(2, Spade): "; test_compare c3D c2S;
  print_string "Compare Card(2, Spade) with Card(3, Diamond): "; test_compare c2S c3D;
  print_newline ();

  print_endline "--- Min and Max ---";
  print_string "Max Card(3, Diamond) / Card(2, Heart): "; print_card (Card.max c3D c2H);
  print_string "Min Card(3, Diamond) / Card(2, Heart): "; print_card (Card.min c3D c2H);
  print_string "Max Card(2, Spade) / Card(2, Heart): "; print_card (Card.max c2S c2H);
  print_newline ();

  print_endline "--- Best ---";
  print_string "Best of all cards: "; print_card (Card.best Card.all);
  print_string "Best of empty list: ";
  (try print_card (Card.best [])
   with Invalid_argument e -> print_endline ("Exception caught: " ^ e));
  print_newline ();

  print_endline "--- Color Checks ---";
  print_endline ("isOf Diamond (As, Diamond): " ^ string_of_bool (Card.isOf cAD Card.Color.Diamond));
  print_endline ("isOf Club (As, Diamond): " ^ string_of_bool (Card.isOf cAD Card.Color.Club));
  print_endline ("isSpade (2, Spade): " ^ string_of_bool (Card.isSpade c2S));
  print_endline ("isHeart (2, Spade): " ^ string_of_bool (Card.isHeart c2S))
