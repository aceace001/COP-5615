
#time "on"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit
open System.Security.Cryptography
open System.Text

let system = ActorSystem.Create("FSharp")

type workerMsg = 
    | Work of int

type MasterMsg = 
    | Assignjob of int

let sha256Hash (input : string) =
    use s256 = SHA256.Create()
    Encoding.ASCII.GetBytes(input)
    |> s256.ComputeHash
    |> Seq.map (fun c -> c.ToString("x2"))
    |> Seq.reduce (+)

let ascii (s : string) = 
    let start:char = ' '
    let ends:char = '~'
    
    let startstring:string = string start
    let endstring:string = string ends

    let ss:char = s.[s.Length - 1]
   
    if (ss = ends) then
        s + startstring

    else
        let sss:int = int ss + 1
        s.[0..(s.Length - 2)] + string (Convert.ToChar(sss))

let a = ascii("aaa~")

let master = spawn system "master" <| fun mailbox ->
    let rec loop() = actor {
        let! msg = mailbox.Receive()

        match msg with
        |Assignjob(a) ->

        
    }


let input:int = int fsi.CommandLineArgs.[1]
let boss = (master <? Assignjob(input))

Async.RunSynchronously(boss,-1) |> ignore

