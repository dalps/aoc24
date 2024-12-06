module P = CCParse
module R = Re

let memory =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let dodont =
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let str_do = "do()"
let str_dont = "dont()"

(* gives up as soon as there's a bad pair after "mul" :( *)
let q =
  let open P in
  many (many_until ~until:(string "mul") any_char *> U.pair U.int U.int)

let p = P.(string "mul" *> U.pair U.int U.int)

let mul =
  R.(
    seq [ str "mul"; char '('; rep1 R.digit; char ','; rep1 R.digit; char ')' ])

let muldodont = R.(alt [ str str_do; mul; str str_dont ])

type instr = Pair of int * int | Do | Dont

let mk_pair (x, y) = Pair (x, y)
let mk_do = Do
let mk_dont = Dont

let pi : instr P.t =
  P.(
    pure mk_pair <*> p
    <|> (pure mk_do <* exact str_do)
    <|> (pure mk_dont <* exact str_dont))

let keep_dos : instr list -> (int * int) list =
  let open CCList in
  let open CCFun in
  fold_filter_map
    (fun switch -> function
      | Do -> (true, None)
      | Dont -> (false, None)
      | Pair (x, y) when switch -> (switch, Some (x, y))
      | _ -> (switch, None))
    true
  %> snd

let get_pairs input : (int * int) list =
  let open CCList in
  (let+ s = R.matches (R.compile mul) input in
   let open CCResult in
   let* x, y = P.parse_string p s in
   (* [+] map, [*] map & flatten *)
   return (x, y))
  |> keep_ok

let mull_it_over = CCList.fold_left (fun acc (x, y) -> acc + (x * y)) 0

let solve1 =
  let open CCFun in
  let open CCList in
  get_pairs %> mull_it_over

let solve2 =
  let open CCList in
  let open CCFun in
  R.matches (R.compile muldodont)
  %> map (P.parse_string pi)
  %> keep_ok %> keep_dos %> mull_it_over
