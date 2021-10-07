open System
open Akka
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit

let system = System.create "system" (Configuration.defaultConfig())

(*
initialization
*)

printfn("Please input your project name, numNodes, one topology, and one algorithm: (eg. project2 10 full gossip)")
let mutable args = Console.ReadLine()
let arg = args.Split ' '
let mutable numNodes:int = 0
numNodes <- int(arg.[1])                          // numNodes
let mutable topology = string(arg.[2])            // topology
let mutable algorithm = string(arg.[3])           // algorithm
let mutable actorStates = [||]
let mutable workersList = [||]
let termination = 10    // for gossip

type BossMsg =
    | BossMsg of int
    | GossipTermination of int
    
type WorkerMsg =
    | GossipWorkermsg of int
    | PushSumWorkermsg of int 

(*
create full network topology
*)
let fullTopology (index: int) =
//    let random = System.Random()
    let mutable a = System.Random().Next(numNodes)
    while a = index do
        a <- System.Random().Next(numNodes)
    a
    
let worker (mailbox: Actor<_>) =
    let mutable count = 0 
    let rec loop() = actor {
        let! message = mailbox.Receive()
        match message with
        | GossipWorkermsg(idx) ->
            count <- count + 1
            actorStates.[idx] <- count
            
            if (actorStates |> Array.min) < termination then
                (*
                match one topology
                *)
                match topology with
                | "full" ->
//                    printfn("full network topology is selected")
                    let randNeighbor = fullTopology idx  
                    workersList.[randNeighbor] <! GossipWorkermsg(randNeighbor)
                | "3D" ->
                    printfn("2D topology is selected")
                | "line" ->
                    printfn("line is selected")
                | "imp3D" ->
                    printfn("imperect 2D is selected")
                | _ ->
                    printfn("ERROR: Please choose one of these topology: full, 3D, line, imp3D")
            else
//                printfn("else")
                select "akka://system/user/SupervisorActor" system  <! GossipTermination(1)
        return! loop()
    }
    loop()
    
let SupervisorActor (mailbox: Actor<_>) =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let rec loop() = actor {
        let! message = mailbox.Receive()
        match message with
        |BossMsg(start) ->
            printfn("call BossMsg")
            workersList <- [| for i in 1..numNodes -> spawn system (string i) worker |]
            actorStates <- [| for i in 1..numNodes -> 0 |]
            match algorithm with
            | "gossip" ->
                workersList.[start] <! GossipWorkermsg(start)
            | "push_sum" ->
                printfn("push_sum")
            | _->
                printfn("ERROR: Algorithm does not exist. Please try again!")

        | GossipTermination(numRumor)->
            stopWatch.Stop()
            printfn("Total run time is %f ms") stopWatch.Elapsed.TotalMilliseconds
        
        return! loop()
    }
    loop()
//let actorRef = spawn system "SupervisorActor" SupervisorActor
let main() =
//    printfn("Please input your project name, numNodes, one topology, and one algorithm: (eg. project2 10 full gossip)")
    let mutable checkArgs = true 
//    let mutable args = Console.ReadLine()
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
    let actorRef = spawn system "SupervisorActor" SupervisorActor 
    match algorithm with
        |"gossip"->
            printfn("in gossip")
            match topology with
                | "full"->
                    printfn("full topology")
                | "3D" ->
                    printfn("3D grid")
                | "line" ->
                    printfn("line topology")
                | "imp3D" ->
                    printfn("imperect 3D topology")
                |_->
                    checkArgs <- false 
                    printfn("ERROR: Topology does not exist. Please try that again!")
        |"push_sum"->
            printfn("push_sum is here")
            match topology with
                | "full"->
                    printfn("full topology")
                | "3D" ->
                    printfn("3D grid")
                | "line" ->
                    printfn("line topology")
                | "imp3D" ->
                    printfn("imperect 3D topology")
                |_->
                    checkArgs <- false 
                    printfn("ERROR: Topology does not exist. Please try that again!")
        |_->
            checkArgs <- false 
            printfn("ERROR: Algorithm does not exist. Please try that again!")
            
    if checkArgs then
        actorRef <! BossMsg 0
           
    
main()

printfn "Press any key to exit"
let res = System.Console.ReadKey()
