#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.Remote"
#r "nuget: Akka.TestKit"

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Security.Cryptography
open System.Text

let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
                debug : {
                    receive : on
                    autoreceive : on
                    lifecycle : on
                    event-stream : on
                    unhandled : on
                }
            }
            remote {
                helios.tcp {
                    hostname = ""127.0.0.1""
                    port = 9001
                }
            }
        }")


let system = ActorSystem.Create("RemoteFSharp", configuration)
let mutable count = 0
let numactor = 8
let mutable result = null

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
               result <! "Done!"

        return! loop()
    }
    loop()

let masteref = spawn system "master" master

let echoserver =
    spawn system "Echoserver"
    <| fun mailbox ->
        let rec loop() =
            actor {
                let! message = mailbox.Receive()
                
                match message with 
                | "Works" -> 
                    masteref <! Assignjob(4)    
                    result <- mailbox.Sender()
                return! loop()
            }
        loop()

system.WhenTerminated.Wait()
