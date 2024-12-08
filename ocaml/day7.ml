open Utils
open F

let p = P.(many @@ both (U.int <* char ':') (many (space *> U.int)))

let get_input f = P.parse_file p f |> R.get_or_failwith

let show_binop ~symbol op x y =
  pr "%d %s %d\n" x symbol y;
  op x y

let digits_of_int n =
  let rec go acc n = if n > 0 then go ((n mod 10) :: acc) (n / 10) else acc in
  if n = 0 then [ 0 ] else go [] n

let int_of_digits ds =
  let open CCInt in
  let rec go mul = function
    | [] -> 0
    | d :: ds -> (d * (10 ** mul)) + go (mul - 1) ds
  in
  go (L.length ds - 1) ds

let ( || ) x y = digits_of_int x @ digits_of_int y |> int_of_digits

let operators =
  [
    show_binop ~symbol:"+" ( + );
    show_binop ~symbol:"*" ( * );
    show_binop ~symbol:"||" ( || );
  ]

let combobulate =
  let open L in
  let rec go = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: xs -> operators <*> go xs <*> [ x ]
  in
  rev %> go

let solve input =
  let open L in
  get_input input
  |> filter_map (fun (result, ns) ->
         if mem result (combobulate ns) then Some result else None)
  |> fold_left ( + ) 0
