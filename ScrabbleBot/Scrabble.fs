namespace FreshFade

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open MultiSet



// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =
    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> debugPrint (sprintf "\n%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =

    
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          customBoard: Map<coord, uint32>
          dict: ScrabbleUtil.Dictionary.Dict
          numberOfPlayers: uint32
          playerNumber: uint32
          PlayerTurn: uint32
          hand: MultiSet.MultiSet<uint32>
          canChange: bool }

    let mkState b sb d np pn pt h tl =
        { board = b
          customBoard = sb
          dict = d
          numberOfPlayers = np
          playerNumber = pn
          PlayerTurn = pt
          hand = h
          canChange = tl }

    let getPairFromSet (set:Set<char*int>) : char*int = 
        match (Set.toList set) with
        | x::xs -> x
        | [] -> debugPrint "WTF"; failwith "Should not happen!"

    let addToHand (hand: MultiSet<uint32>) (newPieces: list<uint32 * uint32>) : MultiSet<uint32> =
        List.fold (fun acc (x, k) -> add x k acc) hand newPieces

    let removeFromHand (hand: MultiSet<uint32>) (piecesToBeRemoved: MultiSet<uint32>) : MultiSet<uint32> =
        subtract hand piecesToBeRemoved

    let updateCustomBoard ms boardMap pieces = 
        let newMs = 
            List.map 
                (fun msElem -> 
                    let coord = fst msElem
                    let id = fst (snd msElem)
                    let charVal = fst (snd (snd msElem))
                    let points = snd (snd (snd msElem))
                    if id = 0u
                    then 
                        List.fold 
                            (fun acc key -> 
                                let result = Map.tryFind key pieces 
                                match result with
                                | Some r -> if fst (getPairFromSet r) = charVal then (coord, (key, (charVal, points))) else acc
                                | None -> acc
                            ) msElem (pieces.Keys |> Seq.cast |> Seq.toList)
                    else
                        msElem
                ) ms
        List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) boardMap newMs

    //Takes the boardMap, finds all vertical starters, and returns them as a list of truples: (coord:StartingPointOfStarter, coord:Direction, list<uint32>:ListOfTilesBeforeStarter)
    let getAllStarters (boardMap: Map<coord, uint32>) : (coord * coord * (uint32) list * uint32) list =
        let keys = (boardMap.Keys |> Seq.cast |> List.ofSeq)
        //If tile above c is clear return true, else return false
        let abovePredicate (c: coord) : bool =
            let cAbove = (fst c, snd c - 1): coord
            let tileAbove = Map.tryFind (cAbove) boardMap
            if tileAbove = None then true else false
        //If tile below c is clear return true, else return false
        let belowPredicate (c: coord) : bool =
            let cBelow = (fst c, snd c + 1): coord
            let tileBelow = Map.tryFind (cBelow) boardMap
            if tileBelow = None then true else false
        //If tile to the left of c is clear return true, else return false
        let leftPredicate (c: coord) : bool =
            let cLeft = (fst c - 1, snd c): coord
            let tileLeft = Map.tryFind (cLeft) boardMap
            if tileLeft = None then true else false
        //If tile to the right of c is clear return true, else return false
        let rightPredicate (c: coord) : bool =
            let cRight = (fst c + 1, snd c): coord
            let tileRight = Map.tryFind (cRight) boardMap
            if tileRight = None then true else false
        //Recursively get letters above tile
        let rec getLettersAboveCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c, snd c - 1)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersAboveCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc

        let rec getLettersBelowCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c, snd c + 1)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersBelowCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc
        
        let rec getLettersLeftOfCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c-1, snd c)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersLeftOfCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc

        let rec getLettersRightOfCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c + 1, snd c)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersRightOfCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc        

        let rec getVerticalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c, snd c + 1)
            if
                not (belowPredicate c) //Der er allerede et bogstav, så kan være ligeglade med dem der ligger til højre og venstre for det
            then 
                getVerticalLength coordToInvestigate acc
            else //Der er ikke et bogstav, så vi skal også lige tjekke til højre og venstre for næste brik
                if 
                    leftPredicate coordToInvestigate
                    && rightPredicate coordToInvestigate
                    && acc < 7u
                then
                    getVerticalLength coordToInvestigate (acc + 1u)
                else
                    acc
        
        let rec getRevVerticalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c, snd c - 1)

            if
                not (abovePredicate c)
            then
                getRevVerticalLength coordToInvestigate acc
            else
                if
                    leftPredicate coordToInvestigate
                    && rightPredicate coordToInvestigate
                    && acc < 7u
                then
                    getRevVerticalLength coordToInvestigate (acc + 1u)
                else
                    acc
        
        let rec getHorizontalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c + 1, snd c)
            if
                not (rightPredicate c)
            then
                getHorizontalLength coordToInvestigate acc
            else
                if 
                    abovePredicate coordToInvestigate
                    && belowPredicate coordToInvestigate
                    && acc < 7u
                then
                    getHorizontalLength coordToInvestigate (acc + 1u)
                else
                    acc
        
        let rec getRevHorizontalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c - 1, snd c)
            if
                not (leftPredicate c)
            then
                getRevHorizontalLength coordToInvestigate acc
            else
                if
                    abovePredicate coordToInvestigate
                    && belowPredicate coordToInvestigate
                    && acc < 7u
                then
                    getRevHorizontalLength coordToInvestigate (acc + 1u)
                else
                    acc
        
        let rec verticalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if belowPredicate c && abovePredicate c then
                [ (c, ((0, 1): coord), [ Map.find c boardMap ], getVerticalLength c 0u) ]
            else if belowPredicate c && not (abovePredicate c) then
                [ (c, ((0, 1): coord), getLettersAboveCoord c [ Map.find c boardMap ], getVerticalLength c 0u) ]
            else
                []
        
        let rec reverseVerticalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if belowPredicate c && abovePredicate c then
                [ (c, ((0, -1): coord), [ Map.find c boardMap ], getRevVerticalLength c 0u) ]
            else if abovePredicate c && not (belowPredicate c) then
                [ (c, ((0, -1): coord), getLettersBelowCoord c [ Map.find c boardMap ], getRevVerticalLength c 0u) ]
            else
                []
        
        let rec horizontalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if rightPredicate c && leftPredicate c then
                [ (c, ((1, 0): coord), [ Map.find c boardMap ], getHorizontalLength c 0u) ]
            else if rightPredicate c && not (leftPredicate c) then
                [ (c, ((1, 0): coord), getLettersLeftOfCoord c [ Map.find c boardMap ], getHorizontalLength c 0u) ]
            else
                []
        
        let rec reverseHorizontalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if rightPredicate c && leftPredicate c then
                [ (c, ((-1, 0): coord), [ Map.find c boardMap ], getRevHorizontalLength c 0u) ]
            else if leftPredicate c && not (rightPredicate c) then
                [ (c, ((-1, 0): coord), getLettersRightOfCoord c [ Map.find c boardMap ], getRevHorizontalLength c 0u) ]
            else
                []
        
        let verticalStarters = List.fold (fun acc c -> List.append acc (verticalPredicateHandler c)) [] keys
        let horizontalStarters = List.fold (fun acc c -> List.append acc (horizontalPredicateHandler c)) [] keys
        let reverseVerticalStarters = List.fold (fun acc c -> List.append acc (reverseVerticalPredicateHandler c)) [] keys
        let reverseHorizontalStarters = List.fold (fun acc c -> List.append acc (reverseHorizontalPredicateHandler c)) [] keys
        verticalStarters @ horizontalStarters @ reverseVerticalStarters @ reverseHorizontalStarters
    
    // let dict = empty () |> insert "BE" |> insert "BETIDED"
    // lookup "BETIDED" dict

    let getStartPoints (boardMap: Map<coord, uint32>) : (coord * coord * (uint32) list * uint32) list =
        getAllStarters boardMap

    //getReadyDict xs (snd (Dictionary.step (Map.find x pieces) d)) pieces
    let rec getReadyDict (letters:list<uint32>) (dict : option<Dictionary.Dict>) (pieces:Map<uint32, tile>) :ScrabbleUtil.Dictionary.Dict = 
        match letters, dict with
        //Case 1: Step into d with bottom of l
        | x :: xs, Some d ->
            let result = (Dictionary.step (fst (getPairFromSet (Map.find x pieces))) d)
            //debugPrint (sprintf "\nresult (Dict): %A\n" result)
            match result with
            | Some r -> getReadyDict xs (Some (snd r)) pieces
            | None -> debugPrint "should not happen. PLEASE!!!"; failwith "should not happen. PLEASE!!!"
        //Case 2: No more letters, return dictionary-level.
        | [], Some d -> d
        | _, None -> debugPrint "should not happen. PLEASE!!!"; failwith "should not happen. PLEASE!!!"
    
    let getLettersFromStarter s = 
        match s with
        | (_, _, letters, _) -> letters

    let removeLetter letter letters = 
        let index = List.tryFindIndex (fun x -> x = letter) letters
        match index with
        | Some(i) -> List.removeAt i letters
        | None -> letters
    
    let removeTile tile tiles =
        let index = List.tryFindIndex (fun x -> fst x = fst tile) tiles
        match index with
        | Some i -> List.removeAt i tiles
        | None -> tiles

    let progressCoord (coord:coord) starter :coord =
        let _, direction, _, _ = starter
        let dirx, diry = direction
        let coordx, coordy = coord
        if dirx = 1
        then (coordx + 1, coordy)
        else
            if diry = 1
            then (coordx, coordy + 1)
            else 
                if dirx = -1
                then (coordx - 1, coordy)
                else
                    if diry = -1
                    then (coordx, coordy - 1)
                    else
                        (coordx, coordy)

    let getPairFromTile (set:tile) : char*int = 
        match (Set.toList set) with
        | x::xs -> x
        | [] -> debugPrint "should not happen"; failwith "Should not happen!"

    //Should be named findPossibleMoves
    let findPossibleContinuations (state:state) (dict:Dictionary.Dict) (letters:uint32 list) (pieces:Map<uint32, tile>) (starter:coord * coord * list<uint32> * uint32) : list<list<(int * int) * (uint32 * (char * int))>> =
        //TODO: Maybe fix duplicate words

        let _, _, _, possibleLength = starter
        let tilesFromLetters = List.map (fun id -> (id, Map.find id pieces)) letters

        let rec newAux coord (currentMove:list<(int * int) * (uint32 * (char * int))>) (tiles:list<uint32 * tile>) auxDict : list<list<(int * int) * (uint32 * (char * int))>> =
            if ((uint32) currentMove.Length) >= possibleLength then [] else
            let wordAtCoord = Map.tryFind coord state.customBoard
            List.fold (fun moves tile -> 
            //1. Try to find out if a letter has already been placed at the current coord
                match wordAtCoord with
                | None -> 
                    if Set.count (snd tile) > 1 && List.fold (fun boolAcc tile -> if (fst tile) = 0u && Set.count (snd tile) = 1 then false else boolAcc) true tiles
                    then 
                        Set.fold (fun wildcardAcc wildcardTile -> wildcardAcc @ (newAux coord currentMove ((0u, Set.empty.Add wildcardTile)::(removeTile tile tiles)) auxDict)) moves (snd tile)
                    else 
                        //2. If no letter has been placed at the current coord, continue searching through the trie to find possible moves.
                        let stepResult = Dictionary.step (fst (getPairFromSet (snd tile))) auxDict
                        match stepResult with
                            //3.1 If a word is found, append the the new word to the accumulator and continue the search with the new word. 
                            | Some r when (fst r) -> 
                                let nextCoord = progressCoord coord starter
                                let nextCoordTile = Map.tryFind nextCoord state.customBoard
                                match nextCoordTile with
                                | Some _ ->
                                    let newMove = List.append currentMove [coord, (fst tile, getPairFromSet(snd tile))]
                                    moves @ (newAux nextCoord newMove (removeTile tile tiles) (snd r))
                                | None ->
                                    let newMove = List.append currentMove [coord, (fst tile, getPairFromSet(snd tile))]
                                    (newMove::moves) @ (newAux nextCoord newMove (removeTile tile tiles) (snd r))
                            //3.2 If a word is not found, but the letter is legal, continue the search with the new word.
                            | Some r when not (fst r) ->
                                let newMove = List.append currentMove [coord, (fst tile, getPairFromSet(snd tile))]
                                moves @ (newAux (progressCoord coord starter) newMove (removeTile tile tiles) (snd r))
                            //3.3 If letter is illegal, return the moves currently found through this path list.
                            | None -> moves
                | Some letterAtCoordId -> 
                    //3. If a letter HAS been placed at the current coord, step into the dictionary with that letter and keep the search going (Note: Nothing is being added to currentMove, as we are not placing a new tile ourselves)
                    let letterAtCoord = Map.find letterAtCoordId pieces
                    let letterAtCoordAsChar = fst (getPairFromSet letterAtCoord)
                    let stepResult = Dictionary.step letterAtCoordAsChar auxDict
                    match stepResult with
                        //3.1 If a word is found, append the the new word to the accumulator and continue the search with the new word. 
                        | Some r when (fst r) -> 
                            currentMove::moves @ (newAux (progressCoord coord starter) currentMove tiles (snd r))
                        | Some r when not (fst r) ->
                            moves @ (newAux (progressCoord coord starter) currentMove tiles (snd r))
                        //3.3 If letter is illegal, return empty list.
                        | None -> moves
            ) [] tiles

        let starterCoord, _, _, _ = starter
        newAux (progressCoord starterCoord starter) [] tilesFromLetters dict

    //Finds the longest move from a list of moves
    let findLongestMove (moves:list<list<'a>>) : list<'a> = 
        if List.isEmpty moves 
        then List.empty
        else List.maxBy (fun move -> move.Length) moves

    //Gets a list of possible words for a given starter
    let getLongestStarterOption (starter:coord * coord * list<uint32> * uint32) (state:state) (pieces:Map<uint32, tile>) : list<(int * int) * (uint32 * (char * int))> =
        //2. Get letters as list so they can be folded over.
        let letters = MultiSet.toList state.hand
        let aux starter =
            match starter with
            |_, (x, y), _, _ when x < 0 || y < 0 -> 
                match (Dictionary.step '>' state.dict) with 
                | Some(false, dict) -> dict 
                | _ -> failwith "should never happen"
            | _ -> state.dict 
        //1. Step in to the dictionary with the letters of the starter.
        let readyDict = getReadyDict (getLettersFromStarter starter) (Some (aux starter)) pieces
        //3. Get a list of possible continuations of the starter
        let possibleMoves = findPossibleContinuations state readyDict letters pieces starter
        findLongestMove possibleMoves
        
    //Finds all possible moves and returns longest one
    let getMove (starters: list<coord * coord * list<uint32> * uint32>) (state:state) (pieces:Map<uint32, tile>) : list<(int * int) * (uint32 * (char * int))> =
        //1. Folds over all starters and returns all possible moves. 
        let allMoves = List.fold (fun (acc:list<list<(int * int) * (uint32 * (char * int))>>) starter -> (getLongestStarterOption starter state pieces)::acc) [] starters
        //2. Finds the longest AKA the best move
        findLongestMove allMoves

    //Makes a starter that that starts in (-1, 0), moves to the right, has no previous letters, and has max-length of 7. Then gets longest option for that starter.  
    let getFirstMove (state:state) (pieces:Map<uint32, tile>) =
        let starter:coord * coord * list<uint32> * uint32 = ((-1, 0), (1, 0), [], 7u)
        getLongestStarterOption starter state pieces
    
    let board st = st.board
    let customBoard st = st.customBoard
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand

    let printStatus st : unit =
        debugPrint (
            sprintf
                "\nTotal players: %d | Our number: %d | total turns: %d | current player turn: %d | Enough tiles: %b \n"
                st.numberOfPlayers
                st.playerNumber
                st.PlayerTurn
                (st.PlayerTurn % st.numberOfPlayers)
                st.canChange
        )

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =
        let rec aux (st: State.state) =
            let totalTilesLeft = 100u
            let itIsMyTurn = st.playerNumber = (st.PlayerTurn % st.numberOfPlayers)
            if itIsMyTurn then
                State.printStatus st
                //forcePrint $"Player {st.playerNumber}: Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n"
                debugPrint (sprintf "Player %d has hand: \n" st.playerNumber)
                Print.printHand pieces (State.hand st)

                //If customBoard is empty, no move has been played
                if st.customBoard.IsEmpty
                then 
                    //Get the best word for first move and play it
                    let move = State.getFirstMove st pieces
                    if move.IsEmpty || move.Length < 3 
                    then
                        debugPrint (sprintf "Player %d: Trying to change tiles in hand\n" st.playerNumber) // keep the debug lines. They are useful.
                        if st.canChange 
                        then send cstream (SMChange (toList st.hand))
                        else send cstream (SMPass)
                    else
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" st.playerNumber move) // keep the debug lines. They are useful.
                        send cstream (SMPlay move)
                else
                    //Get possible starting points for words. If there are none
                    let startingPoints = State.getStartPoints st.customBoard
                    //Get the best move as a list<(int * int) * (uint32 * (char * int))
                    let move = State.getMove startingPoints st pieces
                    if move.IsEmpty 
                    then 
                        debugPrint (sprintf "Player %d -> Server: Trying to change tiles in hand\n" st.playerNumber ) // keep the debug lines. They are useful.
                        if st.canChange
                        then send cstream (SMChange (toList st.hand))
                        else send cstream (SMPass)
                    else
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" st.playerNumber move) // keep the debug lines. They are useful.
                        send cstream (SMPlay move)

            let msg: Response = recv cstream

            if not itIsMyTurn then debugPrint (sprintf "Player %d: <- Server:\n" st.playerNumber ) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                // debugPrint (sprintf "\nnewPieces %A\n" (newPieces))
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                //Update hand

                //let newPiecesWithTwoWildcards = newPieces.Tail.Tail |> List.append [(0, 2 )]

                let piecesToBeRemoved: MultiSet<uint32> = List.fold (fun acc (_, (id, (_, _))) -> add id 1u acc) MultiSet.empty ms
                let handAfterRemove = State.removeFromHand st.hand piecesToBeRemoved
                let handAfterAdd = State.addToHand handAfterRemove newPieces
                //Update board
                let updatedCustomBoard = State.updateCustomBoard ms st.customBoard pieces
                //Make new state
                let st' =
                    State.mkState
                        st.board
                        updatedCustomBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u)
                        handAfterAdd
                        st.canChange
                aux st'
            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //Update board
                let updateCustomBoard = State.updateCustomBoard ms st.customBoard pieces
                //Make new state
                let st' =
                    State.mkState
                        st.board
                        updateCustomBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u)
                        st.hand
                        st.canChange
                aux st'
            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                let st' =
                    State.mkState
                        st.board
                        st.customBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u) // Remember that when reading the value we have to
                        st.hand
                        st.canChange
                // This state needs to be updated
                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM(CMChange(pid, numberOfTiles)) ->
                let st' =
                    State.mkState
                        st.board
                        st.customBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u) // Remember that when reading the value we have to
                        st.hand
                        st.canChange
                // This state needs to be updated
                aux st'
            | RCM(CMChangeSuccess(newTiles)) -> 
                let emptyHand:MultiSet<uint32> = MultiSet.empty 
                let handAfterAdd = State.addToHand emptyHand newTiles

                let st' =
                    State.mkState
                        st.board
                        st.customBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u) // Remember that when reading the value we have to
                        handAfterAdd
                        st.canChange
                // This state needs to be updated
                aux st'
            | RCM(CMPassed pid) -> 
                let st' =
                    State.mkState
                        st.board
                        st.customBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u) // Remember that when reading the value we have to
                        st.hand
                        st.canChange
                // This state needs to be updated
                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                let numberOfGEPNEP = 
                    List.fold (fun acc error -> 
                        match error with
                        | GPENotEnoughPieces(changeTiles, availableTiles) -> 
                            acc + 1
                        | err -> 
                            debugPrint (sprintf "Gameplay Error:\n%A" err)
                            acc
                    ) 0 err
                if numberOfGEPNEP > 0
                then 
                    let st' =
                        State.mkState
                            st.board
                            st.customBoard
                            st.dict
                            st.numberOfPlayers
                            st.playerNumber
                            (st.PlayerTurn + 1u) // Remember that when reading the value we have to
                            st.hand
                            false
                    aux st'
                else
                    aux st
        
        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () ->
            playGame
                cstream
                tiles
                (State.mkState board Map.empty dict numPlayers (playerNumber - 1u) (playerTurn - 1u) handSet true)
