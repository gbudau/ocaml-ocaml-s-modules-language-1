let () =
  Random.self_init ();

  let rec print_string_list = function
    | [] -> print_newline ()
    | [head] -> print_endline head
    | head :: tail ->
        print_string (head ^ ", ");
        print_string_list tail
  in

  print_endline "--- Testing Deck.Card.Value / Color";
  let sample_card = Deck.Card.newCard Deck.Card.Value.T2 Deck.Card.Color.Spade in
  print_endline (Deck.Card.toString sample_card);
  print_newline ();

  let d1 = Deck.newDeck () in
  print_endline "--- Deck 1 ---";
  print_string_list (Deck.toStringList d1);
  print_string_list (Deck.toStringListVerbose d1);
  print_newline ();

  let d2 = Deck.newDeck () in
  print_endline "--- Deck 2 --- (should be in different order)";
  print_string_list (Deck.toStringList d2);
  print_string_list (Deck.toStringListVerbose d2);
  print_newline ();

  print_endline "--- Draw from Deck 1 ---";
  let (card1, deck1) = Deck.drawCard d1 in
  print_endline ("Drawn: " ^ Deck.Card.toString card1);
  print_endline "Remaining deck 1:";
  print_string_list (Deck.toStringList deck1);

  print_endline "--- Test empty deck ---";
  (* We draw recursively until the deck is empty and throws the exception *)
  let rec draw_all d =
    let (_, remaining_deck) = Deck.drawCard d in
    draw_all remaining_deck
  in
  
  try 
    draw_all (Deck.newDeck ())
  with Failure e -> print_endline ("Caught expected Failure: " ^ e);
()
