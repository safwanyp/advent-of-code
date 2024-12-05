(* Define types for directions *)
type direction = { dx: int; dy: int }

(* Get character at position, returning None if out of bounds *)
let get_char grid x y =
  if x >= 0 && y >= 0 && y < Array.length grid && x < String.length grid.(0) then
    Some grid.(y).[x]
  else None

(* Check if a word exists starting from position in given direction *)
let check_word grid x y (dir: direction) word =
  let word_len = String.length word in
  let rec check_pos i =
    if i = word_len then true
    else
      match get_char grid (x + dir.dx * i) (y + dir.dy * i) with
      | Some c when c = word.[i] -> check_pos (i + 1)
      | _ -> false
  in
  check_pos 0

(* Get all possible directions *)
let get_directions () =
  let dirs = [|-1,-1; -1,0; -1,1; 0,-1; 0,1; 1,-1; 1,0; 1,1|] in
  Array.map (fun (dx, dy) -> {dx; dy}) dirs

(* Count all occurrences of word in the grid *)
let count_word grid word =
  let height = Array.length grid in
  let width = String.length grid.(0) in
  let directions = get_directions () in
  let count = ref 0 in

  (* Check each starting position *)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      Array.iter (fun dir ->
        if check_word grid x y dir word then
          incr count
      ) directions
    done
  done;
  !count

let solve input =
  (* Convert input string to array of strings *)
  let grid = input |> String.split_on_char '\n'
                  |> List.filter (fun s -> String.length s > 0)
                  |> Array.of_list in
  count_word grid "XMAS"

let () =
  let input = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX" in
  let result = solve input in
  Printf.printf "%d\n" result
