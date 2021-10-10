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

let termination = 10    // for gossip

type BossMsg =
    | BossMsg of int
    | GossipTermination of int
    | Pushstart of int
    | Pushs
    | Pushcomplete of float
    
type WorkerMsg =
    | GossipWorkermsg of int
    | PushSumWorkermsg of int 
    | Push of double * double
    | Pushsumnode 
    | Init of int [] * int 


let Fulltopo(network: byref<_>, numNodes: int) = 
    for i = 0 to numNodes-1 do
        let mutable arr: int [] = Array.empty
        for j = 0 to numNodes-1 do
            if i <> j then arr <- Array.append arr [|j|]
        network <- Array.append network [|arr|]

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

            let neighborworker = select ("akka://system/user/worker" + string neighborsArr.[Random().Next(neighborsArr.Length)]) system

            neighborworker <! Push(s/2.0, w/2.0)
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
            printfn "%.10f" ratio

            if count = numactor then
                printfn "Finished push-sum. total time:%f" stopwatch.Elapsed.TotalMilliseconds
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

    let nodeActArr = Array.init numNodes (fun index -> spawn system ("worker" + string index) worker)
    let mutable network: int [] [] = Array.empty
    
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    
    let mutable lastTimestamp = stopwatch.Elapsed.TotalMilliseconds
    printfn("time: %f") lastTimestamp

    let actorRef = spawn system "SupervisorActor" SupervisorActor 

    match topology with
    | "full" -> Fulltopo(&network, numNodes)


    for i = 0 to numNodes - 1 do
        nodeActArr.[i] <! Init (network.[i], i)

    match algorithm with
        |"gossip" ->
            printfn("gossip is here")
        |"push_sum"->
            printfn("push_sum is here")
            actorRef <! Pushstart(numNodes)
            
            while true do
                if (stopwatch.Elapsed.TotalMilliseconds - lastTimestamp) >= float(300) then
                    printfn("time: %f") stopwatch.Elapsed.TotalMilliseconds
                    lastTimestamp <- stopwatch.Elapsed.TotalMilliseconds
                    actorRef <! Pushs
        |_->
            
            printfn("ERROR: Algorithm does not exist. Please try that again!")
    

main()

