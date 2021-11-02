open System
open System.Security.Cryptography
open Akka
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Text

let system = System.create "system" (Configuration.defaultConfig())

let sha1 (input : string) =
    use s1 = SHA1.Create()
    Encoding.ASCII.GetBytes(input)
    |> s1.ComputeHash
    |> Seq.map (fun c -> c.ToString("x2"))
    |> Seq.reduce (+)

let r = Random()
let ranStr n = 
    let chars = Array.concat([[|' ' .. '~'|]])
    let s = Array.length chars in String(Array.init n (fun _ -> chars.[r.Next s])) 

printfn("Please input your project name, numNodes, and request: (eg. project3 1000 10)")
let mutable args = Console.ReadLine()
let arg = args.Split ' '
let mutable numNodes:int = 0
numNodes <- int(arg.[1])                          
let mutable request = string(arg.[2])           

let worker (mailbox: Actor<_>) =
    let rec loop() = actor {
        let! message = mailbox.Receive()
        match box message with
        |:?



        return! loop()
    }
    loop()


let SupervisorActor (mailbox: Actor<_>) =
            let mutable counter = 0
            let mutable totalhop = 0
            let rec loop() = actor {
                let! message = mailbox.Receive()
                let sender = mailbox.Sender()
                match box message with
                | :? string as start ->
                    if start = "start" then
                        start
                | :? int as hop ->
                    totalhop <- totalhop + hop
                    counter <- counter + 1
                    if counter = numNodes  then
                        printfn "Average hop is: %f" ((float totalhop) / (float counter))
                        Environment.Exit 1

                return! loop()
            }
            loop()
        


let main() =

    let actorRef = spawn system "SupervisorActor" SupervisorActor 
    let noderef = spawn system "worker" worker


    SupervisorActor  <! "start"
    
    System.Console.ReadLine() |> ignore


    0 
