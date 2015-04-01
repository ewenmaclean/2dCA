type chordel =
  RelNote of int | AbsNote of int | Root of int
type chord = 
   Nil | Cons of (chordel * chord) 

let rec findroot  music = 
  match music with
    | Nil -> None
    | Cons(Root x,_) -> Some x
    | Cons(RelNote _,t) -> findroot t
    | Cons(AbsNote _,t) -> findroot t

let rec calcnoteslist root music = 
   match music with
     | Nil -> []
     | Cons(Root x,t) -> calcnoteslist root t
     | Cons(RelNote 0,t) -> calcnoteslist root t
     | Cons(RelNote x,t) -> ((x+root) mod 12)::(calcnoteslist root t)
     | Cons(AbsNote x,t) -> if (x=root) then (calcnoteslist root t)
       else (x::(calcnoteslist root t))

let rec gettexstring l = 
  let get_val x = 
  match x with
    | 0 -> "c"
    | 1 -> "_d"
    | 2 -> "d"
    | 3 -> "_e"
    | 4 -> "e"
    | 5 -> "f"
    | 6 -> "^f"
    | 7 -> "g"
    | 8 -> "_a"
    | 9 -> "a"
    | 10 -> "_b"
    | 11 -> "b" 
    | _ -> "" 
  in
  match l with 
    | [] -> ""
    | h::[] -> "\\hu{"^(get_val h)^"} &\n"
    | h1::h2::t -> ("\\zh{"^(get_val h1)^"}")^(gettexstring (h2::t))

let calcnotesfromlist root list = 
  let getbass r = 
    match r with 
      | 0 -> "J"
      | 1-> "^J"
      | 2 -> "K"
      | 3 -> "_L"
      | 4 -> "L"
      | 5 -> "F"
      | 6 -> "^F"
      | 7 -> "G"
      | 8 -> "_A"
      | 9 -> "A"
      | 10 -> "_B"
      | 11 -> "B"
      | _ -> ""
  in
  let gt a b = 
    if (a=b) then 0 else 
      (if (a>b) then 1 else -1)
  in
  let sortlist = List.sort gt list in
  let stave2 = (gettexstring sortlist) in
  "\\ha{"^(getbass root)^"} |"^stave2
  
