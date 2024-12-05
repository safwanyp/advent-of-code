module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

(* List utility functions *)
let rec take n lst =
  if n <= 0 then []
  else match lst with
    | [] -> []
    | x::xs -> x :: take (n-1) xs

let rec drop n lst =
  if n <= 0 then lst
  else match lst with
    | [] -> []
    | _::xs -> drop (n-1) xs

let find_index pred lst =
  let rec aux i = function
    | [] -> None
    | x::xs -> if pred x then Some i else aux (i+1) xs
  in
  aux 0 lst

(* Parse input functions *)
let parse_rule line =
  match String.split_on_char '|' line with
  | [before; after] -> (int_of_string before, int_of_string after)
  | _ -> failwith "Invalid rule format"

let parse_update line =
  String.split_on_char ',' line
  |> List.map int_of_string

(* Build adjacency list for topological sort *)
let build_graph rules pages =
  (* Filter rules to only include those where both pages are present *)
  rules |> List.fold_left (fun graph (before, after) ->
    if List.mem before pages && List.mem after pages then
      IntMap.update before
        (function
          | None -> Some (IntSet.singleton after)
          | Some set -> Some (IntSet.add after set))
        graph
    else
      graph
  ) IntMap.empty

(* Topological sort using Kahn's algorithm *)
let topological_sort graph pages =
  let incoming = IntMap.fold (fun _from to_set acc ->
    IntSet.fold (fun to_node acc ->
      IntMap.update to_node
        (function
          | None -> Some 1
          | Some count -> Some (count + 1))
        acc
    ) to_set acc
  ) graph IntMap.empty in

  (* Init queue with nodes that have no incoming edges *)
  let initial_queue = List.filter (fun page ->
    not (IntMap.mem page incoming)
  ) pages in

  let rec kahn result queue incoming_count =
    match queue with
    | [] -> result
    | node :: rest ->
        (* Get the nodes that this one points to *)
        let neighbors = match IntMap.find_opt node graph with
          | None -> IntSet.empty
          | Some set -> set
        in

        (* Process each neighbour *)
        let new_queue, new_incoming =
          IntSet.fold (fun neighbor (q, inc) ->
            match IntMap.find_opt neighbor inc with
            | None -> (q, inc)  (* Skip if no incoming count *)
            | Some count ->
                let new_count = count - 1 in
                let new_inc = if new_count = 0
                  then IntMap.remove neighbor inc
                  else IntMap.add neighbor new_count inc in
                let new_q = if new_count = 0
                  then q @ [neighbor]
                  else q in
                (new_q, new_inc)
          ) neighbors (rest, incoming_count)
        in
        kahn (result @ [node]) new_queue new_incoming
  in

  kahn [] initial_queue incoming

(* Check if order is valid *)
let is_valid_order rules pages =
  let find_index x = List.find_index ((=) x) pages in
  List.for_all (fun (before, after) ->
    if List.mem before pages && List.mem after pages then
      match find_index before, find_index after with
      | Some i, Some j -> i < j
      | _ -> false
    else true
  ) rules

let get_middle lst =
  let len = List.length lst in
  List.nth lst (len / 2)

let solve input =
  let sections = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in
  let rules_end = find_index (fun line -> not (String.contains line '|')) sections in

  match rules_end with
  | None -> failwith "No updates section found"
  | Some idx ->
      let rules = take idx sections |> List.map parse_rule in
      let updates = drop idx sections |> List.map parse_update in

      updates
      |> List.filter (fun update -> not (is_valid_order rules update))
      |> List.map (fun pages ->
          let graph = build_graph rules pages in
          let sorted = topological_sort graph pages in
          get_middle sorted)
      |> List.fold_left (+) 0

let example = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

let () =
  let result = solve example in
  Printf.printf "%d\n" result
