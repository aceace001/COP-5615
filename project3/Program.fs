open System
open System.Security.Cryptography
open System.Text;
open Akka.FSharp

let system = System.create "system" (Configuration.defaultConfig())

type Message =
    | Start of string
    | Successor of int * int * int 

let mutable (keys: int list)= []
let mutable (nodes: int list) = [] 
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

    
let fix_fingers (noden:int) (nodes1:int list) = 
    let mutable fingerTable = []
    let mutable NodeIdx = 0
    let mutable index = 0
    let size = nodes1.Length
   
    while NodeIdx < size && index < int(m) do
        let finger = int(float (noden) + (2.0 ** float(index) % (2.0 ** m)))
        if nodes1.[NodeIdx] < finger then
            while NodeIdx < size && nodes1.[NodeIdx] < finger do
                NodeIdx <- NodeIdx + 1
            if NodeIdx = size then
                NodeIdx <- 0
            fingerTable <- List.append fingerTable [NodeIdx]
            
        NodeIdx <- NodeIdx + 1
        index <- (int)index + 1
    fingerTable

let getfingerTable (nodeId: int)  =
    let sortNodes = List.sort nodes 
    let fingerTable = fix_fingers nodeId sortNodes
    fingerTable

let lookup (id: int) (fingerTable:int list) (keyId: int) =
    let successor = fingerTable.[0]   // the next node on the identifier table
    let mutable successorIdx = id
    let mutable nextNode = -1
    let mutable keyIdx = keyId 
    let sortNodes = List.sort nodes

    if successorIdx < sortNodes.Length then 
        successorIdx <- sortNodes.[successor]
    if (keyIdx <= successorIdx && keyIdx > id) || (successorIdx = 0 && (id < keyIdx || successorIdx >= keyIdx)) then
        nextNode <- fingerTable.[0]

    let m = fingerTable.Length - 1
    for i = m downto 0 do
        let nodeId = sortNodes.[fingerTable.[i]]
        if nodeId < keyIdx && (keyIdx < id || nodeId > id) && nextNode = -1 then
            nextNode <- fingerTable.[i]

    if nextNode = -1 then
        nextNode <- fingerTable.[m]

    nextNode

let chordActor (id: int) (keyList: int list) = spawn system (string id) <| fun mailbox ->
    let mutable integratedKeyList = keyList  
    let rec loop() = actor {
        let! msg = mailbox.Receive() 
        match msg with
            | Start(phrase) ->
                system.Scheduler.Advanced.ScheduleRepeatedly (TimeSpan.FromMilliseconds(0.0), TimeSpan.FromMilliseconds(500.0), fun () -> 
                    let random = Random() 
                    let randomKeyIndex = random.Next(keys.Length)
                    let mutable randomKey = keys.[randomKeyIndex]

                    let fingerTable = getfingerTable id 
                    let nextNodeIndex = lookup id fingerTable randomKey 
                    
                    actorList.[nextNodeIndex] <! Successor(id, randomKey, 0)
                )             
            | Successor(originalID, keyk,hops) -> 
                let keyHash = sha1(keyk |> string) // should we be comparing key hash or the key id? adding this for now
                let newHops = hops+1 
                let mutable keyFound = false

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
                        if (hopsList.Length >= (numnodes * request))
                        then 
                            let mutable sum = 0
                            for num in hopsList do
                                sum <- sum + num 
                            
                            let average = float sum / (float hopsList.Length)
                            printfn "Total Hops: %d\nAverage hops per requests: %f" hopsList.Length average
                            Environment.Exit 0
                    )
                else 
                    let fingerTable = getfingerTable id 
                    let nextNodeIndex = lookup id fingerTable keyk 

                    actorList.[nextNodeIndex] <! Successor(originalID, keyk, newHops)
        return! loop()
    }
    loop()  

let rec findSuccessors first res key node =
    let add key node = 
      match Map.tryFind node res with
      | None -> Map.add node [node; key] res
      | Some l -> Map.add node (l @ [key]) res
    match key, node with 
    | [], _ ->  
      res |> Map.toList |> List.map snd
    | key::ks, [] -> 
      findSuccessors first (add key first) ks []
    | key::ks, node::ns when node < key -> 
      findSuccessors first res (key::ks) ns
    | key::ks, node::ns -> 
      findSuccessors first (add key node) ks (node::ns)

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

    let nodes1 = List.sort nodes
    let keys1 = List.sort keys
    mappings <-  findSuccessors (List.head nodes1) Map.empty keys1 nodes1

    for node in nodes1 do 
        let mutable keyList = [] 
        for nodeMappings in mappings do
            if (nodeMappings.[0] = node) then 
                for i in 1 .. nodeMappings.Length-1 do 
                    keyList <- List.append keyList [nodeMappings.[i]]
        
        actorList <- List.append actorList [chordActor node keyList] 
        
    for actor in actorList do
        actor <! Start("Start")
    
    System.Console.ReadLine() |> ignore
main()
