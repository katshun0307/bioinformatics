type vertex = int * int

type res = Result of string * string * int

let pp_result = function
  | Result(s1, s2, i) -> print_string("(" ^ s1 ^ ", " ^ s2 ^ ", " ^ string_of_int i ^ ")")

let seq0 = "abcdefg"
let seq1 = "fabcdef"
let null_char = "-"
let reward_score = -1
let default_score = 1

let combine s t = 
  match s, t with
    Result(s1, s2, s3), Result(t1, t2, t3) -> Result(s1 ^ t1, s2 ^ t2, s3 + t3)

let flip f x y = f y x

let get_parent v =
  let rec sanitize l = 
    match l with
    | (x, y) as top :: rest ->
      if x - 1 >= 0 && y - 1 >= 0 then top :: (sanitize rest) else sanitize rest
    | [] -> [] in
  match v with
  | (x, y) -> sanitize [(x-1, y-1); (x-1,y); (x, y-1)]

let get_min_res lst = 
  let rec loop l out =
    let Result(_, _, current) = out in
    (match l with
     | Result(a, b, c) as top :: rest -> 
       if c <= current then loop rest top
       else loop rest out
     | [] -> out ) in
  let r = Result("", "", 1000) in
  loop lst r

let rec distance src target = 
  match src, target with
  | (x, y), (w, z) -> 
    let (a, b) as diff = (w - x, z - y) in
    let c0 = Char.escaped seq0.[x] in
    let c1 = Char.escaped seq1.[y] in
    match diff with
    | (1, 1) -> if c0 = c1 then Result(c0, c1, reward_score) else Result(c0, c1, default_score)
    | (0, 1) -> Result(null_char, c1, default_score)
    | (1, 0) -> Result(c0, null_char, default_score)
    | (0, 0) -> Result("", "", 0)
    | _ -> 
      (* middles *)
      let parents = get_parent target in
      (* src -> middle *)
      let induction_distances = List.map (distance src) parents in
      (* middle -> target *)
      let hop_1_distances = List.map (flip distance target) parents in
      let all_paths = List.map2 combine induction_distances hop_1_distances in
      let min = get_min_res all_paths in min

let haha = distance (0, 0) (6, 6);;
pp_result haha;;

let test_res_list = Result("", "", 1) :: Result("", "", 5) :: []

let hoge = get_min_res test_res_list;;