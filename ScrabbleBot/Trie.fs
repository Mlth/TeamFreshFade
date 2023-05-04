
module Dictionary
    type Dict = TrieNode of bool * Map<char, Dict> // TrieNode represents a node in the trie data structure
                                              // bool indicates whether the node represents the end of a word or not
                                              // Map<char, Dict> is the lookup table from characters to the next level in the trie
    let empty () =
        TrieNode (false, Map.empty) // Create an empty TrieNode

    let rec insert (s: string) (dict: Dict) : Dict =

        let reverseString = ">" + (new string(s.ToCharArray() |> Array.rev))

        let rec aux s (dict:Dict): Dict =
            match s with
            | "" -> match dict with TrieNode(_, d) -> TrieNode(true, d) // If the string is empty, create a new TrieNode that represents the end of a word
            | str ->
                let head = str.[0]
                let tail = str.[1..]
                let (isEnd, children) = match dict with TrieNode(b, c) -> (b, c)
                let child =
                    match Map.tryFind head children with // Check if the current character already exists in the lookup table
                    | Some t -> t
                    | None -> empty () // If it doesn't, create a new empty TrieNode
                let updatedChild = aux tail child // Recursively insert the rest of the string into the child node
                TrieNode (isEnd, children |> Map.add head updatedChild) // Add the updated child node to the lookup table
        aux s dict |> aux reverseString

    let rec lookup (s: string) (dict: Dict) =
        match s with
        | "" -> // If the string is empty, check if the current TrieNode represents the end of a word
            match dict with
            | TrieNode (isEnd, _) -> isEnd
        | str ->
            let head = str.[0]
            let tail = str.[1..]
            let (isEnd, children) = match dict with | TrieNode(isEnd, children) -> (isEnd, children)
            match (Map.tryFind head (children)) with // Check if the current character exists in the lookup table
            | Some t -> lookup tail t // Recursively lookup the rest of the string in the child node
            | None -> false // If it doesn't, the string is not in the dictionary

    let step (c: char) (dict: Dict) =
        let children = match dict with | TrieNode(_, d) -> d
        match (Map.tryFind c children) with // Check if the current character exists in the lookup table
        | Some (TrieNode(b, di)) -> Some(b, TrieNode(b, di))
        | None -> None // If it doesn't, return None    