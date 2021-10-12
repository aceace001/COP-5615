#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit

let system = System.create "system" (Configuration.defaultConfig())

printfn("Please input your project name, numNodes, one topology, and one algorithm: (eg. project2 10 full gossip)")
let mutable args = Console.ReadLine()
let arg = args.Split ' '
let mutable numNodes:int = 0
numNodes <- int(arg.[1])                          // numNodes
let mutable topology = string(arg.[2])            // topology
let mutable algorithm = string(arg.[3])       // algorithm

type BossMsg =
    | Gossipstart of int
    | Pushstart of int
    | Pushs
    | Pushcomplete of float
    | Gossipcomplete
    
type WorkerMsg =
    | Gossip
    | Push of double * double
    | Pushsumnode 
    | Init of int [] * int 

let worker (mailbox: Actor<_>) =
    let mutable count = 1 
    let mutable s: double = 0.0
    let mutable w: double = 1.0
    let mutable neighborsArr: int [] = Array.empty
    let supervisor = select "akka://system/user/SupervisorActor" system
    let mutable rationow:double = 0.0

    let rec loop() = actor {
        
        let! message = mailbox.Receive()
        match message with

        | Init (neighbors, index)->
            neighborsArr <- Array.append neighborsArr neighbors
            s <- double(index)
            if index <> 0 then
                w <- 0.0

        | Gossip ->
            if count < 10 then
                let neighboract = select ("akka://system/user/worker" + string neighborsArr.[Random().Next(neighborsArr.Length)]) system
                neighboract <! Gossip
                count <- count + 1
            if count = 10 then
                supervisor <! Gossipcomplete

        | Push(sum, weight) ->
            s <- double(s + sum)
            w <- double(w + weight)

        |Pushsumnode ->
            let mutable ratio = double(s/w)
            if (abs(double(rationow - ratio)) <= 10.0**(-10.0)) then
                count <- count + 1
                if count = 3 then
                    supervisor <! Pushcomplete ratio
            else 
                count <- 0

            rationow <- ratio

            let neighboract = select ("akka://system/user/worker" + string neighborsArr.[Random().Next(neighborsArr.Length)]) system

            neighboract <! Push(s/2.0, w/2.0)
            mailbox.Self.Tell(Push(s/2.0, w/2.0))       

        return! loop()
    }
    loop()
    
let SupervisorActor (mailbox: Actor<_>) =
    let mutable numactor = 0
    let mutable count: int = 0 
    
    let stopwatch = new System.Diagnostics.Stopwatch()
    let rec loop() = actor {
        let! message = mailbox.Receive()
        match message with
        |Gossipstart(numN) ->
            stopwatch.Start()
            numactor <- numN
            for i in 0..numactor-1 do
                let node = select ("akka://system/user/worker" + string i) system
                node <! Gossip
        
        | Gossipcomplete ->
            count <- count + 1
            if count = numactor then 
                printfn "Finished gossip. Total time:%f" stopwatch.Elapsed.TotalMilliseconds
                Environment.Exit 1  

        |Pushstart(numN) ->
            stopwatch.Start()
            numactor <- numN
            for i in 0..numactor-1 do
                let node = select ("akka://system/user/worker" + string i) system

                node <! Push (double(i), 1.0)
        
        |Pushs ->
            for i in 0..numactor-1 do
                let node = select ("akka://system/user/worker" + string i) system
                node <! Pushsumnode 

        |Pushcomplete ratio ->
            count <- count + 1
            //printfn "%.10f" ratio

            if count = numactor then
                printfn "Finished push_sum. Total time:%f" stopwatch.Elapsed.TotalMilliseconds
                Environment.Exit 1  
        return! loop()
    }
    loop()

let main() =

    let mutable num = (args.Split ' ').Length
    while (num <> 4) do
        printfn ("number of arguments are incorrect, please try again!")
        args <- Console.ReadLine()
        num <- (args.Split ' ').Length

    let mutable arg0 = string(arg.[0])    // project2

    while (arg0 <> "project2") do
        printfn ("You should input 'project2', please try again!")
        args <- Console.ReadLine()
        arg0 <- (args.Split ' ').[0]

    let creatework =  [for a in 0..numNodes-1 do yield (spawn system ("worker" + string a)) worker]
    //let nodeActArr = (fun index -> spawn system ("worker" + string index) worker)
    
    let mutable topo = Array.empty
    
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    
    let mutable lastTime1 = stopwatch.Elapsed.TotalMilliseconds

    let actorRef = spawn system "SupervisorActor" SupervisorActor 
    let noderef = spawn system "worker" worker

    match topology with
    | "full" -> 
        for i in 0..numNodes-1 do
            let mutable arr = Array.empty
            for j = 0 to numNodes-1 do
                if i <> j then 
                    arr <- Array.append arr [|j|]
            topo <- Array.append topo [|arr|]
    | "line" -> 
        for i in 0..numNodes - 1 do
            let mutable arr = Array.empty
            if i = 0 then 
                arr <- Array.append arr [|i+1|]
            elif i = numNodes - 1 then 
                arr <- Array.append arr [|i-1|] 
            else 
                arr <- Array.append arr [|i-1; i+1|]
            topo <- Array.append topo [|arr|]
            
    | "3D" ->
        let sqrtNumNodes1 = int(ceil((float(numNodes) ** (1.0/3.0))))
        let sqrtNumNodes = sqrtNumNodes1 * sqrtNumNodes1

        printfn "%d %d" sqrtNumNodes1 sqrtNumNodes
        for i = 0 to numNodes - 1 do
            let mutable arr: int [] = Array.empty
            if (i % sqrtNumNodes1) <> 0 then 
                arr <- Array.append arr [|i-1|]
            if ((i % sqrtNumNodes1) < sqrtNumNodes1 - 1)  then 
                arr <- Array.append arr [|i+1|]
            if (i/sqrtNumNodes) < sqrtNumNodes1 - 1 then
                arr <- Array.append arr [|i + sqrtNumNodes|]
            if (i/sqrtNumNodes) <> 0 then
                arr <- Array.append arr [|i - sqrtNumNodes|]
            if ((i%sqrtNumNodes)/sqrtNumNodes1)<>0 then
                arr <- Array.append arr [|i - sqrtNumNodes1|]
            if ((i%sqrtNumNodes)/sqrtNumNodes1)<sqrtNumNodes1-1 then
                arr <- Array.append arr [|i + sqrtNumNodes1|]

            topo <- Array.append topo [|arr|]
    | "imperfect3D" ->
        let sqrtNumNodes1 = int(ceil((float(numNodes) ** (1.0/3.0))))
        let sqrtNumNodes = sqrtNumNodes1 * sqrtNumNodes1

        printfn "%d %d" sqrtNumNodes1 sqrtNumNodes
        for i = 0 to numNodes - 1 do
            let mutable arr: int [] = Array.empty
            if (i % sqrtNumNodes1) <> 0 then 
                arr <- Array.append arr [|i-1|]
            if ((i % sqrtNumNodes1) < sqrtNumNodes1 - 1)  then 
                arr <- Array.append arr [|i+1|]
            if (i/sqrtNumNodes) < sqrtNumNodes1 - 1 then
                arr <- Array.append arr [|i + sqrtNumNodes|]
            if (i/sqrtNumNodes) <> 0 then
                arr <- Array.append arr [|i - sqrtNumNodes|]
            if ((i%sqrtNumNodes)/sqrtNumNodes1)<>0 then
                arr <- Array.append arr [|i - sqrtNumNodes1|]
            if ((i%sqrtNumNodes)/sqrtNumNodes1)<sqrtNumNodes1-1 then
                arr <- Array.append arr [|i + sqrtNumNodes1|]

            let mutable randNeighbor = Random().Next(numNodes)
            arr <- Array.append arr [|randNeighbor|]
            topo <- Array.append topo [|arr|]
    |_->        
        printfn("ERROR: Topology does not exist. Please try that again!")
    
    //printfn "%A" topo

    let mutable lastTime = stopwatch.Elapsed.TotalMilliseconds
    printfn("Topology build time: %f") lastTime

    for i in 0..(numNodes - 1) do
        creatework.Item(i) <! Init (topo.[i], i)

    match algorithm with
        |"gossip" ->
            printfn("gossip is here")
            actorRef <! Gossipstart(numNodes)
            
            while true do
                if (stopwatch.Elapsed.TotalMilliseconds - lastTime1) >= float(300) then
                    lastTime1 <- stopwatch.Elapsed.TotalMilliseconds
                    noderef <! Gossipstart(numNodes)
        |"push_sum"->
            printfn("push_sum is here")
            actorRef <! Pushstart(numNodes)
            
            while true do
                if (stopwatch.Elapsed.TotalMilliseconds - lastTime1) >= float(10) then
                    lastTime1 <- stopwatch.Elapsed.TotalMilliseconds
                    actorRef <! Pushs
        |_->
            
            printfn("ERROR: Algorithm does not exist. Please try that again!")

main()

