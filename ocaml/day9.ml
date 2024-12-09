open Utils
open F

let p = P.(many @@ any_char)

let from_string s =
  let open L in
  s |> P.parse_string p |> R.get_exn >|= CCChar.to_string
  >|= CCInt.of_string_exn

let from_file s =
  let open L in
  s |> P.parse_file p |> R.get_exn >|= CCChar.to_string >|= CCInt.of_string
  |> keep_some

let e1 = from_string "12345"
let e2 = from_string "2333133121414131402"

type block = E | I of int

let string_of_block = function E -> "." | I i -> string_of_int i

let print_blocks_l = print_newline % L.iter (print_string % string_of_block)
let print_blocks_a = print_newline % A.iter (print_string % string_of_block)

let rec expand blocks =
  let open L in
  flatten
  @@
  let* i, b = mapi CCPair.make blocks in
  pure @@ replicate b (if i mod 2 = 0 then I (i / 2) else E)

let filter_empty =
  let open L in
  snd
  % L.fold_filter_map_i
      (fun _ i -> function E -> ((), Some i) | _ -> ((), None))
      ()

let filter_files_r =
  let open L in
  L.foldi (fun acc i x -> match x with I f -> (f, i) :: acc | _ -> acc) []

let checksum =
  A.foldi (fun acc i x -> acc + match x with I x -> i * x | E -> 0) 0

let solve1 input =
  let blocks = from_file input |> expand |> A.of_list in
  let j = ref (A.length blocks - 1) in
  let open A in
  iteri
    (fun i -> function
      | E when i < !j ->
          while !j >= 0 && blocks.(!j) = E do
            decr j
          done;
          swap blocks i !j
      | _ -> ())
    blocks;
  print_blocks_a blocks;
  checksum blocks
