open Utils

let p = P.(many U.int)

let ( <+> ) : 'a list -> 'a list -> 'a list =
 fun l0 l1 ->
  match l0 with [] -> ( match l1 with [] -> [] | _ -> l1) | l0 -> l0

let blink1 (l : int list) =
  let open L in
  let* l = l in
  (if l = 0 then [ 1 ] else [])
  <+> (let digits = digits_of_int l in
       let len = length digits in
       if len mod 2 = 0 then
         [
           take (len / 2) digits |> int_of_digits;
           drop (len / 2) digits |> int_of_digits;
         ]
       else
         [])
  <+> [ l * 2024 ]

let rec blinkn (times : int) l =
  if times > 0 then (
    flush stdout;
    blink1 l |> blinkn (times - 1))
  else
    l

let solve1 input = input |> P.parse_file p |> R.get_exn |> blinkn 25 |> L.length
