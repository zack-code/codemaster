let (>>=) x f = Option.bind f x

type Move = | R | G | B 

type [<Struct>] Pos = Pos of int
type [<Struct>] Christals = Christals of int
    with
    static member  (-) (Christals x,y) = Christals(x-y)
    static member  (+) (Christals x,y) = Christals(x+y)

type ChristalState = {
    positions: Map<Pos, Christals>
    count: Christals
}

module Christals =
    let take pos state =
        match Map.tryFind pos state.positions with
        | Some x -> 
            if x > Christals 1 then
                { positions = Map.add pos (x-1) state.positions
                  count = state.count + 1 }
            else
                { positions = Map.remove pos state.positions
                  count = state.count + 1 }
        | None -> state

type [<Measure>] mv
type [<Measure>] cd

type Troll =
    | Orange
    | Purple

type Condition =
    | Troll of Troll
    | ChristalCount of Christals

type Scroll =
    | Portal
    | Move of int<mv> * Scroll
    | Condition of int<cd> * (bool -> Scroll)

let ( **> ) mv next = Move(mv * 1<mv>, next)
let ( **?> ) cd f = Condition(cd * 1<cd>,f)


type ScrollState =
    {
        scroll: Scroll
        moves: Map<int<mv>, Move>
        conditions: Map<int<cd>,Condition>
    }


module ScrollState =
    let start scroll =
         { moves = Map.empty
           conditions = Map.empty
           scroll = scroll }

    let (|Finish|ApplyMove|ChooseMove|ApplyCondition|ChooseCondition|) state =
        match state.scroll with
        | Portal -> Finish
        | Move(i, next) ->
            match Map.tryFind i state.moves with
            | Some m ->
                ApplyMove(m, fun () -> { state with scroll = next })
            | None ->
                ChooseMove(fun m -> 
                    { state with
                        scroll = next
                        moves = Map.add i m state.moves })
        | Condition(i, next) ->
            match Map.tryFind i state.conditions with
            | Some c ->
                ApplyCondition(i, c, fun b -> { state with scroll = next b })
            | None ->
                ChooseCondition(i,fun c b -> {
                    state with 
                        scroll = next b
                        conditions = Map.add i c state.conditions })

type Map = {
    graph: Move list[][]
    trolls: Map<Pos, Troll>
}

type Level = {
    map: Map
    scroll: Scroll
    start: Pos
    portal: Pos
    christals: int list
    conditions: Condition list
    moves: Move list
}

type StateCapture =
    { pos: Pos
      condition: int<cd>
      moves: Map<Move, int>
      conditions: Condition Set
      christals: ChristalState }

type State = {
    pos: Pos
    availableMoves: Map<Move, int>
    availableConditions: Condition Set
    christals: ChristalState
    scrollState: ScrollState
    level: Level
    captures: StateCapture Set
}


module Scroll =
    let useAction action lst =
        match Map.tryFind action lst with
        | Some x ->
            if x > 1 then
                lst |> Map.add action (x-1)
            else
                lst |> Map.remove action
        | None -> lst

module State =
    open ScrollState
    let start level =
        { pos = level.start
          availableMoves = level.moves |> List.countBy id |> Map.ofList
          availableConditions = level.conditions |> set
          christals = { 
                positions = level.christals |> List.countBy id |> List.map (fun (p,c) -> Pos p, Christals c) |> Map.ofList
                count = Christals 0 }
          scrollState = ScrollState.start level.scroll
          level = level
          captures = Set.empty }

    let possibleMoves state : Move list =
        state.availableMoves
        |> Map.toList
        |> List.map fst

    let possibleConditions state : Condition list =
        state.availableConditions
        |> Set.toList

    let dirs state = 
        let (Pos p) = state.pos 
        state.level.map.graph.[p]

    let move action state =
        dirs state
        |> Array.tryFindIndex (fun possibleActions -> 
            List.contains action possibleActions) 
        |> Option.map Pos

    let tryMove a remove newScroll s = 
        match s |> move a with
        | Some newPos ->
            let newMoves = if remove then s.availableMoves |> Scroll.useAction a else s.availableMoves
            let newChristals = s.christals |> Christals.take newPos
            { s with
                  pos = newPos
                  availableMoves = newMoves
                  christals = newChristals
                  scrollState = newScroll() } |> Some
        | None -> None

    let tryFindTroll t state =
        match Map.tryFind state.pos state.level.map.trolls with
        | Some foundTroll -> t = foundTroll
        | None -> false

    let applyCondition c newScroll s = 
        let met =
            match c with
            | Troll t -> tryFindTroll t s
            | ChristalCount count -> s.christals.count = count
        { s with
            availableConditions = Set.remove c s.availableConditions
            scrollState = newScroll met }

    // let move a next s =  
    //     tryMove a next s |> Option.defaultWith (fun _ -> failwith "Not allowed")
    let capture cid state : StateCapture =
        { pos = state.pos
          condition = cid
          moves = state.availableMoves
          conditions = state.availableConditions
          christals = state.christals }

    let isSuccess state = 
        match state.scrollState.scroll with
        | Portal ->
            state.pos = state.level.portal 
            && Map.isEmpty state.christals.positions
        | _ -> false

    let rec tryPath state =
        match state.scrollState with
        | Finish ->
            if isSuccess state then
                Some state.scrollState
            else
                None
        | ApplyMove (m, next) ->
            state |> tryMove m false next >>= tryPath
        | ChooseMove(next) ->
            state
            |> possibleMoves 
            |> List.tryPick (fun a ->
                state |> tryMove a true (fun () -> next a) >>= tryPath)
        | ApplyCondition(i, c, next) ->
            let cap = capture i state
            match Set.contains cap state.captures with
            | false -> 
                { state with captures = Set.add cap state.captures }
                |> applyCondition c next |> tryPath
            | true -> None
        | ChooseCondition(i,next) ->
            let cap = capture i state
            state
            |> possibleConditions
            |> List.tryPick (fun c ->
                { state with 
                    captures = Set.add cap state.captures }
                    |> applyCondition c (fun b -> next c b) |> tryPath)


let x = []
let b = [B]
let r = [R]
let g = [G]

let bg = [B;G]
let rb = [R;B]
let gr = [G;R]


let scroll1() = 1 **> 2 **> 3 **> 4 **> Portal

let scroll2() = 1 **> 2 **> 3 **> 4 **> 5 **> Portal

let scroll3() = 1 **> 2 **> 3 **> 4 **> 5 **> 6 **> Portal

let rec scroll4() =
    1 **> 2 **> 3 **> 4 **?> function
                             | true -> Portal
                             | false -> scroll4()

let scroll5() = 1 **> 2 **> 3 **> 4 **> 5 **> 6 **> 7 **> Portal

let rec scroll6() =
    1 **> 2 **> 3 **> 4 **> 5 **?> function
                                   | true -> scroll6()
                                   | false -> Portal

let rec scroll7() =
    1 **> cont()
and cont() =
    2 **?> function
        | true -> 3 **> 4 **> 5 **> cont()
        | false -> 6 **> 7 **> Portal

let rec scroll8() =
    1 **?> function
           | true  -> 2 **> join8()
           | false -> 3 **> join8()
and join8() =
    4 **> 5 **?> function
                 | true -> Portal
                 | false -> scroll8() 

let rec scroll9() =
    1 **?> function
          | false -> 2 **> 3 **> 4 **> 5 **> scroll9()
          | true -> 6 **> 7 **> 8 **> Portal

let rec scroll10() =
    1 **> 2 **> 3 **> 4 **> 5 **?> function
                                   | true -> Portal
                                   | false -> scroll10()

let rec scroll11() =
    1 **> 2 **?> function
                 | true  -> 3 **> join11() 
                 | false -> 4 **> join11()
and join11 () =
    5 **> 6 **?> function
                 | true -> Portal
                 | false -> scroll11()

let rec scroll12() =
    1 **?> function
           | true -> 2 **> last()
           | false -> 3 **?> function
                             | true -> 4 **> last()
                             | false -> 5 **> last()
and last() =
    6 **> 7 **?> function
                 | true -> Portal
                 | false -> scroll12()
let map1 = {
    graph = 
        [|  // 0  1  2  3  4  5
            [| x; r; g; x; b; x |] // 0
            [| r; x; b; g; x; x |] // 1
            [| g; b; x; x; x; r |] // 2
            [| x; g; x; x; r; b |] // 3
            [| b; x; x; r; x; g |] // 4
            [| x; x; r; b; g; x |] // 5
        |]
    trolls = Map.ofList [Pos 0, Purple; Pos 5, Orange]
}

let map2 = {
    graph = 
        [|  // 0  1  2  3  4  5
            [| x; b; r; x; g; x |] // 0
            [| b; x; g; x; x; r |] // 1
            [| r; g; x; b; x; x |] // 2
            [| x; x; b; x; r; g |] // 3
            [| g; x; x; r; x; b |] // 4
            [| x; r; x; g; b; x |] // 5
        |]
    trolls = Map.ofList [Pos 1, Orange; Pos 2, Purple]
}

let map3 = {
    graph = 
        [|  // 0  1  2  3  4  5
            [| x;gr; x; b; x; x |] // 0
            [|gr; x; b; x; x; x |] // 1
            [| x; b; x; r; x; g |] // 2
            [| b; x; r; x; g; x |] // 3
            [| x; x; x; g; x;rb |] // 4
            [| x; x; g; x;rb; x |] // 5 
        |]
    trolls = Map.ofList [Pos 0, Purple; Pos 2, Purple; Pos 4, Orange]
}
    
let map4 = {
    graph = 
        [|  // 0  1  2  3  4  5
            [| b;gr; x; x; x; x |] // 0
            [|gr; x; b; x; x; x |] // 1
            [| x; b; x; r; x; g |] // 2
            [| x; x; r; x; g; b |] // 3
            [| x; x; x; g; b; r |] // 4
            [| x; x; g; b; r; x |] // 5 
        |]
    trolls = Map.ofList [Pos 0, Purple; Pos 5, Purple]
}

let map5 = { 
    graph =
        [|  // 0  1  2  3  4  5
            [| x; g; x; x; b; r |] // 0
            [| g; x; r; b; x; x |] // 1
            [| x; x; x; g; x; b |] // 2
            [| x; b; g; x; r; x |] // 3
            [| b; x; x; r; x; g |] // 4
            [| r; x; b; x; g; x |] // 5
        |]
    trolls = Map.ofList [Pos 0, Orange; Pos 3, Purple] 
    }

let map6 = {
    graph =
        [|  // 0  1  2  3  4  5
            [| x; r; x; x; x; x |] // 0
            [| x; x; r; b; g; x |] // 1
            [| r; g; x; x; b; x |] // 2
            [| x; b; g; x; r; x |] // 3
            [| x; x; b; g; x; r |] // 4
            [| x; x; x; r; x; x |] // 5
        |]
    trolls = Map.ofList [Pos 4, Orange; Pos 1, Orange ;Pos 2, Purple]
    }

let map7 = {
    graph =
        [|  // 0  1  2  3  4  5  6  7  8
            [| x; g; b; r; x; x; x; x; x |] // 0
            [| g; x; x; x; r; x; x; x; x |] // 1
            [| b; x; x; g; x; r; x; x; x |] // 2
            [| r; x; g; x; b; x; x; x; x |] // 3
            [| x; r; x; b; x; x; g; x; x |] // 4
            [| x; x; r; x; x; x; b; g; x |] // 5
            [| x; x; x; x; g; b; x; x; r |] // 6
            [| x; x; x; x; x; g; x; x; b |] // 7
            [| x; x; x; x; x; x; r; b; x |] // 8
        |]
    trolls = Map.ofList [Pos 0, Orange]
    }

let map8 = {
    graph = 
        [|  // 0  1  2  3  4  5  6  7  8  9 10 11
            [| r; g; b; x; x; x; x; x; x; x; x; x |] //  0
            [| g; x; x; b; r; x; x; x; x; x; x; x |] //  1
            [| b; g; x; r; x; x; x; x; x; x; x; x |] //  2
            [| x; b; r; x; x; x; g; x; x; x; x; x |] //  3
            [| x; x; x; x; x; b; x; x; g; r; x; x |] //  4
            [| x; x; x; r; b; x; x; x; x; g; x; x |] //  5
            [| x; x; x; x; x; b; x; r; x; x; g; x |] //  6
            [| x; x; g; x; x; x; r; x; x; x; x; b |] //  7
            [| x; x; x; x; g; x; x; x; b; r; x; x |] //  8
            [| x; x; x; x; x; g; x; x; r; x; b; x |] //  9
            [| x; x; x; x; x; x; g; b; x; x; x; r |] // 10
            [| x; x; x; x; x; x; x; b; x; x; r; g |] // 11
        |]
    trolls = Map.ofList [Pos 3, Orange; Pos 9, Orange; Pos 10, Orange]
}

let map9 = {
    graph =
        [|  // 0  1  2  3  4  5  6  7  8  9
            [| x; b; x; x; x; x; g; x; x; r |] // 0 
            [| x; x; r;bg; x; x; x; x; x; x |] // 1
            [| g; x; x; x; b; r; x; x; x; x |] // 2
            [| x; g; x; x; r; x; b; x; x; x |] // 3
            [| x; g; x; x; x; r; x; b; x; x |] // 4
            [| x; x;gr; x; x; x; x; x; b; x |] // 5
            [| g; x; x; x; x; x; x; r; x; b |] // 6
            [| x; x; x; g; x; x; x; x;rb; x |] // 7
            [| x; x; x; x; g; x; x; b; x; r |] // 8
            [| r; x; x; x; x; g; b; x; x; x |] // 9
        |]
    trolls = Map.ofList [ Pos 1, Orange ]
}

let map10 = {
    graph = 
        [|  // 0  1  2  3  4  5  6  7  8  9 10 11
            [| x; r; x; x; x; x; x; g; x; x; x; x |] //  0
            [| r; x; g; x; x; b; x; x; x; x; x; x |] //  1
            [| x; g; r; b; x; x; x; x; x; x; x; x |] //  2
            [| x; x; b; x; r; x; g; x; x; x; x; x |] //  3
            [| x; x; x; r; x; x; x; x; x; x; x; g |] //  4
            [| x; b; x; x; x; x; r; x; g; x; x; x |] //  5
            [| x; x; x; g; x; r; x; x; x; x; b; x |] //  6
            [| b; x; x; x; x; x; x; x; r; x; x; x |] //  7
            [| x; x; x; x; x; g; x; r; x; b; x; x |] //  8
            [| x; x; x; x; x; x; x; x; b; r; g; x |] //  9
            [| x; x; x; x; x; x; b; x; x; g; x; r |] // 10
            [| x; x; x; x; b; x; x; x; x; x; r; x |] // 11
        |]
    trolls = Map.ofList [ Pos 4, Purple; Pos 7, Orange; Pos 10, Orange]
}

let level1 = {
    map = map1
    scroll = scroll1()
    start = Pos 5
    portal = Pos 3
    christals = []
    conditions = []
    moves = [R;R;G;G] }

let level2 = {
    map = map2
    scroll = scroll1()
    start = Pos 3
    portal = Pos 5
    christals = [ 0 ]
    conditions = []
    moves = [R;R;B;B] }

let level3 = {
    map = map3
    scroll = scroll1()
    start = Pos 4
    portal = Pos 0
    christals = [ 1; 5 ]
    conditions = []
    moves = [R;G;B;B] }

let level4 = {
    map = map4
    scroll = scroll2()
    start = Pos 1
    portal = Pos 5
    christals = [ 3; 4 ]
    conditions = []
    moves = [R;R;G;B;B] }

let level5 = {
    map = map5
    scroll = scroll1()
    start = Pos 2
    portal = Pos 5
    christals = [ 3 ]
    conditions = []
    moves = [R;G;B;B] }

let level6 = {
    map = map6
    scroll = scroll2()
    start = Pos 5
    portal = Pos 0
    christals = [ 1;4 ]
    conditions = []
    moves = [R;R;G;B;B] }

let level7 = {
    map = map7
    scroll = scroll2()
    start = Pos 0
    portal = Pos 8
    christals = [ ]
    conditions = []
    moves = [R;R;G;G;B] }

let level8 = {
    map = map8
    scroll = scroll1()
    start = Pos 7
    portal = Pos 4
    christals = []
    conditions = []
    moves = [R;R;G;B] }

let level9 = {
    map = map9
    scroll = scroll2()
    start = Pos 9
    portal = Pos 4
    christals = [0;7]
    conditions = []
    moves = [R;R;G;G;B] }

let level10 = {
    map = map10
    scroll = scroll3()
    start = Pos 2
    portal = Pos 6
    christals = []
    conditions = []
    moves = [R;R;R;G;B;B] }
let level11 = {
    map = map1
    scroll = scroll1()
    start = Pos 2
    portal = Pos 4
    christals = [1]
    conditions = []
    moves = [R;R;G;B]
}

let level12 = {
    map = map2
    scroll = scroll2()
    start = Pos 5
    portal = Pos 0
    christals = []
    conditions = []
    moves = [R;R;G;G;B]
}
let level13 = {
    map = map3
    scroll = scroll2()
    start = Pos 5
    portal = Pos 0
    christals = [4;4]
    conditions = []
    moves = [R;R;R;G;B]
}
let level14 = {
    map = map4
    scroll = scroll2()
    start = Pos 0
    portal = Pos 4
    christals = [4;4;5]
    conditions = []
    moves = [R;G;G;B;B]
}
let level15 = {
    map = map5
    scroll = scroll2()
    start = Pos 4
    portal = Pos 0
    christals = [2;5]
    conditions = []
    moves = [R;R;G;B;B]
}
let level16 = {
    map = map6
    scroll = scroll4()
    start = Pos 1
    portal = Pos 3
    christals = [2;2;2;4;4;4]
    conditions = [ChristalCount (Christals 6)]
    moves = [R;G;B]
}
let level17 = {
    map = map7
    scroll = scroll3()
    start = Pos 3
    portal = Pos 5
    christals = [0;4]
    conditions = []
    moves = [R;G;G;G;B;B]
}
let level18 = {
    map = map8
    scroll = scroll3()
    start = Pos 9
    portal = Pos 2
    christals = [11]
    conditions = []
    moves = [R;G;G;B;B;B]
}
let level19 = {
    map = map9
    scroll = scroll3()
    start = Pos 2
    portal = Pos 8
    christals = [0;4]
    conditions = []
    moves = [R;G;G;B;B;B]
}
let level20 = {
    map = map10
    scroll = scroll5()
    start = Pos 6
    portal = Pos 0
    christals = [2]
    conditions = []
    moves = [R;R;R;G;B;B;B]
}
let level21 = {
    map = map1
    scroll = scroll2()
    start = Pos 0
    portal = Pos 5
    christals = [1;1]
    conditions = []
    moves = [R;G;G;G;B]
}
let level22 = {
    map = map2
    scroll = scroll3()
    start = Pos 5
    portal = Pos 0
    christals = [3;4]
    conditions = []
    moves = [R;G;G;G;G;B]
}
let level23 = {
    map = map3
    scroll = scroll4()
    start = Pos 2
    portal = Pos 4
    christals = [1;3]
    conditions = [ChristalCount (Christals 2)]
    moves = [R;G;B]
}
let level24 = {
    map = map4
    scroll = scroll5()
    start = Pos 0
    portal = Pos 5
    christals = [4;4;5;5]
    conditions = []
    moves = [R;R;G;G;G;G;B]
}
let level45 = {
    map = map5
    scroll = scroll11()
    start = Pos 2
    portal = Pos 3
    christals = [1;1;1 ]
    conditions = [ChristalCount (Christals 3); Troll Purple]
    moves = [R;R;G;B] }

let level46 = {
    map = map6
    scroll = scroll12()
    start = Pos 3
    portal = Pos 4
    christals = [ 0; 1;1;1; 4 ]
    conditions = [ChristalCount (Christals 2); Troll Purple ;ChristalCount (Christals 5)]
    moves = [B;G;R;R] }

let level47 = {
    map = map7
    scroll = scroll7()
    start = Pos 0
    portal = Pos 8
    christals = [1;6] 
    conditions = [ChristalCount (Christals 1)]
    moves = [R;R;G;G;G;B] }

let level50 = {
    map = map10
    scroll = scroll7()
    start = Pos 8
    portal = Pos 11
    christals = [ 4; 4; 5; 11; 11 ]
    conditions = [ChristalCount (Christals 1)]
    moves = [R;G;G;G;B;B] }

let level55 = {
    map = map5
    scroll = scroll11()
    start = Pos 0
    portal = Pos 5
    christals =  [ 1; 1; 4; 5]
    conditions = [ChristalCount (Christals 1); ChristalCount (Christals 4)]
    moves = [R;G;B;B] }

let level60 = {
    map = map10
    scroll = scroll9()
    start = Pos 8
    portal = Pos 9
    christals = [ 2; 9]
    conditions = [ChristalCount (Christals 2)]
    moves = [R;R;R;G;G;B;B] }



let print =
    function
    | None -> printfn "Unable to solve this level"
    | Some (s: ScrollState) ->
        let mv = function | R -> "Red" | G -> "Green" | B -> "Blue"
        let cd = function | Troll t -> sprintf "%A Troll" t | ChristalCount (Christals c) -> sprintf "%dx" c
        let mvs = s.moves |> Map.toSeq |> Seq.map (fun (i,m) -> i/1<mv>, mv m) |> Seq.toList
        let cds = s.conditions |> Map.toSeq |> Seq.map (fun (i,c) -> i/1<cd>, cd c) |> Seq.toList
        mvs @ cds
        |> List.sortBy fst
        |> List.iter (fun (i,s) -> printfn "%d: %s" i s )

let solve game =
    State.start game
    |> State.tryPath
    |> print

#time
solve level1
solve level2
solve level3
solve level4
solve level5
solve level6
solve level7
solve level8
solve level9
solve level10
solve level11
solve level12
solve level13
solve level14
solve level15
solve level16
solve level17
solve level18
solve level19
solve level20
solve level21
solve level22
solve level23
solve level24
solve level45
solve level46
solve level47
solve level50
solve level55
solve level60

#time

let rec repeat() =
    seq {
        yield State.start level45
        yield State.start level46
        yield State.start level47
        yield! repeat()
    }


let a =
    repeat() 
    |> Seq.take 10000
    |> Array.ofSeq
    |> Array.Parallel.map State.tryPath

