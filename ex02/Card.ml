module type Color =
sig
type t = Spade | Heart | Diamond | Club
val all : t list
val toString : t -> string
val toStringVerbose : t -> string
end

module type Value =
sig
type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
val all : t list
val toInt : t -> int
val toString : t -> string
val toStringVerbose : t -> string
val next : t -> t
val previous : t -> t
end

module Color : Color =
struct

  type t = Spade | Heart | Diamond | Club
  
  let all = [Spade ; Heart ; Diamond ; Club] 
  
  let toString (card: t) : string =
    match card with
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"
  
  let toStringVerbose (card: t) : string =
    match card with
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"

  
end

module Value : Value =
struct

  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
  
  let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]
  
  (** Integer representation of a card value, from 1 for T2 to 13 for As *)
  let toInt (card: t) : int =
    match card with
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13
  
  (** returns "2", ..., "10", "J", "Q", "K" or "A" *)
  let toString (card: t) : string =
    match card with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"
  
  (** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
  let toStringVerbose (card: t) : string =
    match card with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"
  
  (** Returns the next value, or calls invalid_arg if argument is As *)
  let next (card: t) : t =
    match card with
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "invalid argument As"
  
  (** Returns the previous value, or calls invalid_arg if argument is T2 *)
  let previous (card: t) : t =
    match card with
    | T2 -> invalid_arg "invalid argument T2"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King

end

type t = Value.t * Color.t

let newCard (value: Value.t) (color: Color.t): t = (value, color)
let allSpades : t list = List.map (fun value -> (value, Color.Spade)) Value.all
let allHearts : t list = List.map (fun value -> (value, Color.Heart)) Value.all
let allDiamonds : t list = List.map (fun value -> (value, Color.Diamond)) Value.all
let allClubs : t list = List.map (fun value -> (value, Color.Club)) Value.all
let all : t list = List.append (List.append (List.append allSpades allHearts) allDiamonds) allClubs

let getValue ((v, _): t) =  v
let getColor ((_, c): t) =  c
let toString ((v, c): t) =
  Printf.sprintf "Card(%s, %s)" (Value.toString v) (Color.toString c)
let toStringVerbose ((v, c): t) =
  Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose v) (Color.toStringVerbose c)

let compare ((v1, _): t) ((v2, _): t) =
  (Value.toInt v1) - (Value.toInt v2)

let max (a: t) (b: t) =
  if (compare a b) >= 0 then a else b

let min (a: t) (b: t) =
  if (compare a b) <= 0 then a else b

let best (cards: t list) : t =
  if List.is_empty cards
    then invalid_arg "invalid argument: empty cards list"
  else
    List.fold_left max (List.nth cards 0) cards

let isOf ((_, c): t) color = c = color
let isSpade (card: t) = isOf card Color.Spade
let isHeart (card: t) = isOf card Color.Heart
let isDiamond (card: t) = isOf card Color.Diamond
let isClub (card: t) = isOf card Color.Club
