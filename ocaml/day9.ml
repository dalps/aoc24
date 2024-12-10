open Utils
open F

let p = P.(many @@ any_char)

let parse s =
  let open L in
  s |> P.parse_file p |> R.get_exn >|= CCChar.to_string >|= CCInt.of_string
  |> keep_some

type block = E | I of int

let string_of_block = function E -> "." | I i -> string_of_int i

let print_memory = print_newline % A.iter (print_string % string_of_block)

let expand raw =
  let open L in
  flatten
  @@
  let* i, b = mapi CCPair.make raw in
  pure @@ replicate b (if i mod 2 = 0 then I (i / 2) else E)

let checksum =
  A.foldi (fun acc i x -> acc + match x with I x -> i * x | E -> 0) 0

let solve1 input =
  let blocks = parse input |> expand |> A.of_list in
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
  print_memory blocks;
  checksum blocks

type file_chunk = { id : int; len : int; start : int }
type empty_chunk = { len : int; start : int }

let file_map =
  let open L in
  snd
  % foldi
      (fun (start, acc) i len ->
        ( len + start,
          cons_when
            (i mod 2 = 0)
            (let id = i / 2 in
             { id; len; start })
            acc ))
      (0, [])

let memory = parse "9.txt" |> expand |> A.of_list

let scan_for_empty (blocks : block array) (f : file_chunk) =
  let open A in
  find_mapi
    (fun start -> function
      | E ->
          if start >= f.start then
            None
          else
            let j = ref start in
            while !j < A.length blocks && blocks.(!j) = E do
              incr j
            done;
            let len = !j - start in
            if len >= f.len then
              Some { start; len }
            else
              None
      | I _ -> None)
    blocks

let solve2 input =
  let raw = parse input in
  let open L in
  let file_dict = file_map raw in
  let memory = expand raw |> A.of_list in
  print_memory memory;
  iter
    (fun (f : file_chunk) ->
      ignore
      @@
      let open O in
      let* e = scan_for_empty memory f in
      A.blit memory f.start memory e.start f.len;
      A.fill memory f.start f.len E;
      pure ())
    file_dict;
  print_memory memory;
  checksum memory
