(* check for MAS pattern (forward or backward) in a given direction *)
let check_mas grid x y dx dy =
  let check_pos x y =
    x >= 0 && y >= 0 && y < Array.length grid && x < String.length grid.(0)
  in

  (* check if a MAS exists starting at this position *)
  let check_forward x y =
    check_pos x y && check_pos (x+dx) (y+dy) && check_pos (x+2*dx) (y+2*dy) &&
    grid.(y).[x] = 'M' &&
    grid.(y+dy).[x+dx] = 'A' &&
    grid.(y+2*dy).[x+2*dx] = 'S'
  in

  (* check if a SAM exists starting at this position *)
  let check_backward x y =
    check_pos x y && check_pos (x+dx) (y+dy) && check_pos (x+2*dx) (y+2*dy) &&
    grid.(y).[x] = 'S' &&
    grid.(y+dy).[x+dx] = 'A' &&
    grid.(y+2*dy).[x+2*dx] = 'M'
  in

  check_forward x y || check_backward x y

(* check if a valid X pattern exists with an A at the center *)
let check_x grid center_x center_y =
  if center_y >= 0 && center_y < Array.length grid &&
     center_x >= 0 && center_x < String.length grid.(0) &&
     grid.(center_y).[center_x] = 'A' then
    (* check both diagonal directions from the center A *)
    let has_top_left_to_bottom_right =
      check_mas grid (center_x-1) (center_y-1) 1 1 ||  (* Forward *)
      check_mas grid (center_x+1) (center_y+1) (-1) (-1)  (* Backward *)
    in
    let has_top_right_to_bottom_left =
      check_mas grid (center_x+1) (center_y-1) (-1) 1 ||  (* Forward *)
      check_mas grid (center_x-1) (center_y+1) 1 (-1)  (* Backward *)
    in
    has_top_left_to_bottom_right && has_top_right_to_bottom_left
  else
    false

(* count all X-MAS patterns in the grid *)
let count_x_mas_patterns grid =
  let count = ref 0 in
  for y = 0 to Array.length grid - 1 do
    for x = 0 to String.length grid.(0) - 1 do
      if check_x grid x y then
        incr count
    done
  done;
  !count

let solve input =
  let grid = input
    |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length s > 0)
    |> Array.of_list
  in
  count_x_mas_patterns grid

let input = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

let () =
  let result = solve input in
  Printf.printf "%d\n" result
