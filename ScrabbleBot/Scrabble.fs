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
          hand: MultiSet.MultiSet<uint32> }

    let mkState b sb d np pn pt h =
        { board = b
          customBoard = sb
          dict = d
          numberOfPlayers = np
          playerNumber = pn
          PlayerTurn = pt
          hand = h }

    let addToHand (hand: MultiSet<uint32>) (newPieces: list<uint32 * uint32>) : MultiSet<uint32> =
        List.fold (fun acc (x, k) -> add x k acc) hand newPieces

    let removeFromHand (hand: MultiSet<uint32>) (piecesToBeRemoved: MultiSet<uint32>) : MultiSet<uint32> =
        subtract hand piecesToBeRemoved

    let updateCustomBoard ms boardMap =
        List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) boardMap ms

    //Takes the boardMap, finds all vertical starters, and returns them as a list of truples: (coord:StartingPointOfStarter, coord:Direction, list<uint32>:ListOfTilesBeforeStarter)
    let getVerticalStarters (boardMap: Map<coord, uint32>) : (coord * coord * (uint32) list * uint32) list =
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

        let rec getPossibleLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c, snd c + 1)

            if
                belowPredicate coordToInvestigate
                && leftPredicate coordToInvestigate
                && rightPredicate coordToInvestigate
                && acc < 7u
            then
                getPossibleLength coordToInvestigate (acc + 1u)
            else
                acc

        let rec predicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if belowPredicate c && abovePredicate c then
                [ (c, ((0, 1): coord), [ Map.find c boardMap ], getPossibleLength c 0u) ]
            else if belowPredicate c && not (abovePredicate c) then
                [ (c, ((0, 1): coord), getLettersAboveCoord c [ Map.find c boardMap ], getPossibleLength c 0u) ]
            else
                []

        List.fold (fun acc c -> List.append acc (predicateHandler c)) [] keys

    let getStartPoints (boardMap: Map<coord, uint32>) : (coord * coord * (uint32) list * uint32) list =
        debugPrint (sprintf "\nboardMap %A\n" (boardMap))
        getVerticalStarters boardMap

    let getStarterOptions (starter:coord * coord * list<uint32> * uint32) (state:state) : list<list<(int * int) * (uint32 * (char * int))>> = failwith "not implmented"
        //1. Step in to the dictionary with the letters of the starter

        //2. Try to step further and construct a word with the letters contained in our hand
    
    let findLongestMove (moves:list<list<(int * int) * (uint32 * (char * int))>>) : list<(int * int) * (uint32 * (char * int))> = 
        List.maxBy (fun move -> move.Length) moves
    
    let getMove (starters: list<coord * coord * list<uint32> * uint32>) (state:state) : list<(int * int) * (uint32 * (char * int))> = 
        let allMoves = List.fold (fun (acc:list<list<(int * int) * (uint32 * (char * int))>>) starter -> List.append (getStarterOptions starter state) acc) [] starters
        findLongestMove allMoves
    
    let board st = st.board
    let customBoard st = st.customBoard
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand

    let printStatus st : unit =
        debugPrint (
            sprintf
                "\nTotal players: %d | Our number: %d | total turns: %d | current player turn: %d \n"
                st.numberOfPlayers
                st.playerNumber
                st.PlayerTurn
                (st.PlayerTurn % st.numberOfPlayers)
        )

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =
        let rec aux (st: State.state) =
            let itIsMyTurn = st.playerNumber = (st.PlayerTurn % st.numberOfPlayers)

            if itIsMyTurn then
                State.printStatus st
                forcePrint
                    $"Player {st.playerNumber}: Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n"
                Print.printHand pieces (State.hand st)
                //If board is empty, bruteforce word using tiles on hand and coords for placemement.

                //else
                let startingPoints = State.getStartPoints st.customBoard
                if itIsMyTurn then
                    debugPrint (sprintf "Player %d: \nStartingpoints: %A \n" st.playerNumber (startingPoints))
                // remove the force print when you move on from manual input (or when you have learnt the format)
                let input = System.Console.ReadLine()
                let move = RegEx.parseMove input
                if itIsMyTurn then
                    debugPrint (
                        sprintf "Player %d: Player %d -> Server:\n%A\n" st.playerNumber (State.playerNumber st) move
                    ) // keep the debug lines. They are useful.
                send cstream (SMPlay move)

            let msg: Response = recv cstream

            if not itIsMyTurn then
                debugPrint (sprintf "Player %d: Player %d <- Server:\n" st.playerNumber (State.playerNumber st)) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                // debugPrint (sprintf "\nnewPieces %A\n" (newPieces))
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                //UPDATE HAND
                let piecesToBeRemoved: MultiSet<uint32> =
                    List.fold (fun acc (_, (id, (_, _))) -> add id 1u acc) empty ms

                let handAfterRemove = State.removeFromHand st.hand piecesToBeRemoved
                let handAfterAdd = State.addToHand handAfterRemove newPieces
                //UPDATE BOARD
                let updatedCustomBoard = State.updateCustomBoard ms st.customBoard

                let st' =
                    State.mkState
                        st.board
                        updatedCustomBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u)
                        handAfterAdd

                aux st'
            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //UPDATE BOARD
                let updatedStinkyBoard = State.updateCustomBoard ms st.customBoard

                let st' =
                    State.mkState
                        st.board
                        updatedStinkyBoard
                        st.dict
                        st.numberOfPlayers
                        st.playerNumber
                        (st.PlayerTurn + 1u)
                        st.hand

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
                // This state needs to be updated
                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                debugPrint (sprintf "Gameplay Error:\n%A" err)
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
                (State.mkState board Map.empty dict numPlayers (playerNumber - 1u) (playerTurn - 1u) handSet)
