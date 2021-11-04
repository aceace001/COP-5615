open System
open System.Security.Cryptography
open System.Text;
open Akka.FSharp
open Akka.Actor
open Akka.Routing

let system = System.create "system" (Configuration.defaultConfig())

type Message =
    | Start of string
    | Successor of int * int * int // key * hopsCount

// list of keys in SHA1 form
// Nodes = [nodens in int form] [42;84..]
// Keys = [keyk in int form] 
let mutable (keys: int list)= []
let mutable (nodes: int list) = []  // node42 "DSHGFIOAHDIFOH2138Y9"
let mutable (mappings: int list list) = [[]]
let mutable actorList = []
let m = 10.0
let mutable (hopsList:int list) = []
let _lock = Object()

printfn("Please input your numNodes, and request: (eg. 1000 10)")
let mutable args = Console.ReadLine()
let arg = args.Split ' '
let mutable numnodes:int = int(arg.[0])
let mutable request = int(arg.[1])    

let sha1 (input : string) =
    use s1 = SHA1.Create()
    Encoding.ASCII.GetBytes(input)
    |> s1.ComputeHash
    |> Seq.map (fun c -> c.ToString("x2"))
    |> Seq.reduce (+)

    
// [3 8]
let checkIfFinished() =
    if (hopsList.Length >= (numnodes * request))
    then 
        let mutable sum = 0
        for num in hopsList do
            sum <- sum + num 
        
        let average = float sum / (float hopsList.Length)
        printfn "Total Hops: %d\nAverage hops per requests: %f" hopsList.Length average
        Environment.Exit 0
    
let fix_fingers (noden:int) (sortedNodes:int list) = 
    let mutable fingerTable = []
    //let sortedNodes = List.sort nodes // [a list of hashes]
    let mutable closestNodeIndex = 0 
    for i in 1..int(m) do
        if closestNodeIndex < sortedNodes.Length
        then
            let finger = int(( float(noden) + (2.0**float(i-1)) ) % (2.0**m)) 
            if sortedNodes.[closestNodeIndex] < finger
            then // keep checking nodes until theres a node bigger than current finger, or we reach final node
                while closestNodeIndex < sortedNodes.Length && sortedNodes.[closestNodeIndex] < finger do
                    closestNodeIndex <- closestNodeIndex + 1
                if closestNodeIndex = sortedNodes.Length // if there are no nodes bigger than finger, than map finger to 1st index
                then closestNodeIndex <- 0
            fingerTable <- List.append fingerTable [closestNodeIndex] 

    fingerTable
(*
get key index 
*)
let getKeyIdx () =
//    let mutable integratedKeyList = keyList
    let random = Random()
    let randKeyIdx = random.Next(keys.Length)
    randKeyIdx 
//    let mutable randKey = keys.[randKeyIdx]
//    while (List.contains randKey integratedKeyList && integratedKeyList.Length <> keys.Length) do
(*
get finger table 
*)
let getfingerTable (nodeId: int)  =
    let sortNodes = List.sort nodes 
    let fingerTable = fix_fingers nodeId sortNodes
    fingerTable
(*
lookup node using finger table 
*)
let lookup (id: int) (fingerTable:int list) (keyId: int) =
    let successor = fingerTable.[0]   // the next node on the identifier table
    let mutable successorIdx = id
    let mutable nextNode = -1
    let mutable keyIdx = keyId 
    let sortNodes = List.sort nodes
    let mutable res = 0
//    if choice = 0 then
//        keyIdx <- getKeyIdx()
    if successorIdx < sortNodes.Length then 
        successorIdx <- sortNodes.[successor]
    if (keyIdx <= successorIdx && keyIdx > id) || (successorIdx = 0 && (id < keyIdx || successorIdx >= keyIdx)) then
        nextNode <- fingerTable.[0]
//        nextNode <- true 
//        res <- fingerTable.[0]
    let m = fingerTable.Length - 1
    for i = m downto 0 do
        let nodeId = sortNodes.[fingerTable.[i]]
        if nodeId < keyIdx && (keyIdx < id || nodeId > id) && nextNode = -1 then
            nextNode <- fingerTable.[i]
//            res <- fingerTable.[i]
    if nextNode = -1 then
        nextNode <- fingerTable.[m]
//        res <- fingerTable.[m]
    nextNode
//    res
            
    
  
let chordActor (id: int) (keyList: int list) = spawn system (string id) <| fun mailbox ->
    // printfn "Created actor with id: %s." id 
    // printfn "My keys are: %A" keyList
    // printfn "My successor is located at %i, and is %i" (fst(successor)) (snd(successor))
    let mutable integratedKeyList = keyList
    
    let rec loop() = actor {
        let! msg = mailbox.Receive() 
        match msg with
            | Start(phrase) ->
                system.Scheduler.Advanced.ScheduleRepeatedly (TimeSpan.FromMilliseconds(0.0), TimeSpan.FromMilliseconds(500.0), fun () -> 
                                       
                        // Don't allow KEYS that Exist
                        // [node1; key1; key2] [node2; key3; key4]
                        // [node1; key1; key2 ... keyn]
                        let random = Random() 
                        let randomKeyIndex = random.Next(keys.Length)
                        let mutable randomKey = keys.[randomKeyIndex]
                        while (List.contains randomKey integratedKeyList && not (integratedKeyList.Length = keys.Length)) do 
                            // printfn "We already had key %i in our keyList %A" randomKey integratedKeyList
                            
                            let randomKeyIndex2 = random.Next(keys.Length)
                            // printfn "randomKeyIndex2: %i" randomKeyIndex2
                            // printfn "Integrated Key List: %A" integratedKeyList
                            randomKey <- keys.[randomKeyIndex2]
//                        let nextNodeIndex = getNodeFromFingerTable id randomKey
                        (*
                        updated starts
                        *)
                        let fingerTable = getfingerTable id 
                        let nextNodeIndex = lookup id fingerTable randomKey 
                        (*
                        updated ends
                        *)
                        //printf "\n req: %d" !sentRequests
                        
                        // printfn "Node %d sending request to Node %d for Key %d " id sortedNodes.[nextNodeIndex] someKey
                        try 
                            actorList.[nextNodeIndex] <! Successor(id, randomKey, 0)
                        with 
                            | _ -> 
//                                let nextNodeIndex = getNodeFromFingerTable id randomKey
                                (*
                                updated starts
                                *)
                                let fingerTable = getfingerTable id 
                                let nextNodeIndex = lookup id fingerTable randomKey 
                                (*
                                updated ends
                                *)
                                actorList.[nextNodeIndex] <! Successor(id, randomKey, 0)
                ) 
                 
            | Successor(originalID, keyk,hops) -> 
                let keyHash = sha1(keyk |> string) // should we be comparing key hash or the key id? adding this for now
                let newHops = hops+1 // keep track of how many hops it takes to find key by itterating by 1
                let originalHash = sha1(string originalID)
                let idHash = sha1(id |> string)
                // If we made it all the way back to the current user
                // Then we can assume the id doesn't exist 
                // printfn "original ID %i" originalID
                // printfn "my Id %i" id
                if (originalHash = idHash) // 
                then
                    // printfn "at original node"
                    lock _lock (fun () -> 
                        hopsList <- List.append hopsList [newHops]
                        // printfn "Key not found after %d hops" newHops
                        checkIfFinished()
                    )
                // Else, we need to check if the currentNode contains our key.
                else 
                    // printfn "HERE. ID: %i Key: %i. %A"id keyk integratedKeyList
                    let mutable keyFound = false
                    //printfn " Key List: %A" keyList
                    //printf " keyBeingSearchedFor %i" keyk
                    

                    // REMINDER: KeyList only has keys that the node has so we can just compare our keyHash to every hash in keyList
                    for key in integratedKeyList do // check if current node contains key we are looking for
                        // printfn "Found the Key."
                        let keyBeingSearchedFor = sha1(key |> string)
                        if keyBeingSearchedFor = keyHash
                        then 
                            keyFound <- true
                    if keyFound
                    then 
                        lock _lock (fun () -> 
                            hopsList <- List.append hopsList [newHops]
                            checkIfFinished()
                        )
                        // send request every second
                    else 
//                        let nextNodeIndex = getNodeFromFingerTable id keyk
//                        printfn ("nextNodeIndex before: %A") nextNodeIndex
//                        let temp = nextNodeIndex
                        (*
                        updated starts
                        *)
                        let fingerTable = getfingerTable id 
                        let nextNodeIndex = lookup id fingerTable keyk 
                                           
//                        printfn ("nextNode and nextNodeIndex: %A , %A") nextNode nextNodeIndex
                        (*
                        updated ends
                        *)
                        //printfn "Next Node Index: %d" nextNodeIndex
                        try 
                            actorList.[nextNodeIndex] <! Successor(originalID, keyk, newHops)
                        with 
                            | _ -> 
//                                let nextNodeIndex = getNodeFromFingerTable id keyk
                                (*
                                updated starts
                                *)
//                                let fingerTable = getfingerTable id 
//                                let nextNodeIndex = lookup id fingerTable keyk 
                                (*
                                updated ends
                                *)
                                actorList.[nextNodeIndex] <! Successor(originalID, keyk, newHops)
        // handle an incoming message
        return! loop() // store the new s,w into the next state of the actor
    }
    loop()  


let rec findSuccessors first res k n =
    // Add a key 'k'  associated with a successor node 'n' to the list
    let add k n = 
      match Map.tryFind n res with
      | None -> Map.add n [n; k] res
      | Some l -> Map.add n (l @ [k]) res
    match k, n with 
    | [], _ ->  
      // If there are no more keys, we return the results
      res |> Map.toList |> List.map snd
    | k::ks, [] -> 
      // If there are no more successors, use the special 'first'
      findSuccessors first (add k first) ks []
    | k::ks, n::ns when n < k -> 
      // If we have a key 'k', but the next node is smaller, skip it
      findSuccessors first res (k::ks) ns
    | k::ks, n::ns -> 
      // Found a key 'k' with a successor 'n' - add it to the list
      findSuccessors first (add k n) ks (n::ns)

let main() = 
    for n in 0..numnodes-1 do
        let r = Random()
        let noden =  r.Next(int (2.0**m - 1.0)) 
        if (not (List.contains noden nodes)) then 
            nodes <- List.append nodes [noden]  

    for k in 0..numnodes/2 do
        let r = Random()
        let keyk = r.Next(int (2.0**m - 1.0))
        if (not (List.contains keyk keys)) then
            keys <- List.append keys [keyk]

    let sortedNodes = List.sort nodes
    let sortedKeys = List.sort keys
    printfn "nodes:%A" sortedNodes
    printfn "keys:%A" sortedKeys

    mappings <-  findSuccessors (List.head sortedNodes) Map.empty sortedKeys sortedNodes

    (*
    for i in 0..sortedKeys.Length-1 do 
        for j in 0..sortedNodes.Length-1 do
            if sortedKeys.[i] <= sortedNodes.[j] && sortedKeys.[i] > sortedNodes.[j-1] then
                printfn "nodeforkey: %d %d" sortedKeys.[i] sortedNodes.[j]
                
                nk <- List.append nk [sortedNodes.[j]]
    printfn "nk:%A" nk
    printfn "sk:%d" sortedKeys.Length
    printfn "mk:%d" nk.Length
    for i in 0..sortedKeys.Length-1 do
        for j in nk do
            if j = mappings.[i].[0] then
                mappings.[i] <- Array.append mappings.[i] [|sortedKeys.[i]|]
    printfn "maps:%A" mappings
                //mappings.[i] <- Array.append mappings.[i] [|sortedKeys.[i]|]
            //elif sortedKeys.[i] > sortedNodes.[j] then
            //    printfn "nodeforkey: %d %d" sortedKeys.[i] sortedNodes.[0]
            //    mappings <- Array.append mappings [|[|sortedNodes.[j]; sortedKeys.[i]|]|]
    //printfn "maps:%A" mappings
    *)
    for node in sortedNodes do // For each Node, we will check if it has a mapping in node mapping.
        let mutable keyList = [] // keyList will be a list of keys that are associated with each actor.
        for nodeMappings in mappings do
            let mappedNode = nodeMappings.[0] //nodeMappings.[0] should be the noden that the keys are mapped to.
            if (mappedNode = node) then 
                for i in 1 .. nodeMappings.Length-1 do 
                    keyList <- List.append keyList [nodeMappings.[i]]
        
        actorList <- List.append actorList [chordActor node keyList] 
        
    for actor in actorList do
        actor <! Start("Start")
    
    System.Console.ReadLine() |> ignore
main()
