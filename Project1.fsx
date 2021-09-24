#time "on"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"

open System
open Akka.Actor
open Akka.FSharp
open System.Security.Cryptography
open System.Text

let system = ActorSystem.Create("FSharp")
let mutable count = 0
let numactor = 8

type WorkerMsg = 
    | Work of int

type MasterMsg = 
    | Assignjob of int
    | Finished

let sha256Hash (input : string) =
    use s256 = SHA256.Create()
    Encoding.ASCII.GetBytes(input)
    |> s256.ComputeHash
    |> Seq.map (fun c -> c.ToString("x2"))
    |> Seq.reduce (+)

let rec leadzeros (h:string) = 
    if h.[0] <> '0' then
        0
    else 
        1 + leadzeros(h.[1..h.Length])

let worker (mailbox:Actor<_>) = 
    let rec loop () = actor {
        let! msg = mailbox.Receive()
        match msg with
        | Work(input) -> 

            let r = Random()
            let ranStr n = 
                let chars = Array.concat([[|' ' .. '~'|]])
                let s = Array.length chars in String(Array.init n (fun _ -> chars.[r.Next s])) 

            for _i in 0..numactor..1000000 do 
                let items = "yuhaoshi" + ranStr(10)
                let hash = sha256Hash(items)
                let count = leadzeros(hash) 
                if count = input then
                    printfn "%s %s" items hash
            
            mailbox.Sender() <! Finished
            
        return! loop ()
    }
    loop ()

let master
    (mailbox:Actor<_>) =
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        match msg with
        |Assignjob(a)-> 
            let creatework = 
                [for a in 1..numactor do yield (spawn system ("actor" + string(a))) worker]
                
            for id in 0..(numactor-1) do
                creatework.Item(id) <! Work (a)
            
        |Finished -> 
            count <- count + 1
            if count = numactor then
               mailbox.Context.System.Terminate() |> ignore

        return! loop()
    }
    loop()


let input = int (fsi.CommandLineArgs |> Seq.item 1)
let masteref = spawn system "master" master
masteref <! Assignjob(input)

system.WhenTerminated.Wait()
