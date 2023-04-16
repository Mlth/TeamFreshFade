module internal MultiSet

    type MultiSet<'a when 'a: comparison> = R of Map<'a, uint32>

    let empty = (R Map.empty)
    let isEmpty (R s) = Map.isEmpty s

    let size (R s) =
        Map.fold (fun acc _ value -> acc + value) 0u s

    let contains a (R s) = Map.containsKey a s

    let numItems a (R s) =
        if Map.containsKey a s then s.[a] else 0u

    let add a (b: uint32) (R s) = R(s.Add(a, (numItems a (R s) + b)))
    let addSingle a s = add a 1u s

    let remove a (n: uint32) (R s) =
        let k = numItems a (R s)

        if not (k = 0u) && k > n then
            R(s.Add(a, (k - n)))
        else
            R(s.Remove a)

    let removeSingle a s =
        if contains a s then remove a 1u s else s

    let fold f acc (R s) = Map.fold f acc s
    let foldBack f (R s) acc = Map.foldBack f s acc

    let ofList a =
        (empty, a) ||> List.fold (fun acc key -> addSingle key acc)

    let toList (R a) =
        (a, [])
        ||> Map.foldBack (fun key value acc -> List.replicate ((int) value) key @ acc)

    let map f (R a) =
        (empty, a) ||> Map.fold (fun acc key value -> add (f key) value acc)

    let union (R s1) (R s2) =
        (s1, s2)
        ||> (Map.fold (fun acc key value ->
            match Map.tryFind key acc with
            | Some w -> Map.add key (max w value) acc
            | _ -> Map.add key value acc))
        |> R

    let sum (R s1) (R s2) =
        (s1, s2)
        ||> (Map.fold (fun acc key value ->
            match Map.tryFind key acc with
            | Some w -> Map.add key (w + value) acc
            | _ -> Map.add key value acc))
        |> R

    let subtract (R s1) (R s2) =
        (s1, s2)
        ||> (Map.fold (fun acc key value ->
            match Map.tryFind key acc with
            | Some w when not (w = 0u) && w > value -> Map.add key (w - value) acc
            | _ -> Map.remove key acc))
        |> R

    let intersection (R s1) (R s2) =
        Map.filter (fun k _ -> Map.containsKey k s1) s2 |> R
