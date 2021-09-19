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

type WorkerMsg = 
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

let rec leadzeros (h:string) = 
    if h.[0] <> '0' then
        0
    else 
        1 + leadzeros(h.[1..h.Length])

let compute inpu = 

    let workunit:int = 10000
    let start:char = ' '
    let startstring:string = string start
    let mutable workends:string = startstring
    

    while (workends.Length <= workunit) do
        let bitcoin:string = "yuhaoshi" + workends
        let hash = sha256Hash(bitcoin)
        let count = leadzeros(hash) 
        ///Seq.length(Seq.filter (fun x' -> x' = '0') hash.[0..workunit])

        if (count = inpu) then
            printfn "%s %s" bitcoin hash

        workends <- ascii(workends)

compute 4




let splitwork a = 
    let numactor = 8
    
    [1..numactor]
    |> List.map(fun id -> 
                    spawn system ("actor" + string(id))
                    <| fun mailbox ->
                        let rec loop() = actor {
                            let! msg = mailbox.Receive()
                            
                            match msg with
                            | Work(input1) -> compute input1
                                              mailbox.Sender() <! "Done"
                            return! loop()
                        }
                        loop())


//let master = spawn system "master" <| fun mailbox ->
//    let rec loop() = actor {
//        let! msg = mailbox.Receive()
//        let mutable numactor = 0
//
//        match msg with
//        |Assignjob(a)-> let re = splitwork a
//                        for r in re do
//                            Async.RunSynchronously(r,-1) |> ignore
//                        mailbox.Sender() <! "Done"
//
//        return! loop()
//    }
//    loop()

let input:int = int fsi.CommandLineArgs.[0]

// input number of leading 0's
//let k:int = int fsi.CommandLineArgs.[1]
let k = int(fsi.CommandLineArgs |> Seq.item 2)
//let boss = (master <? Assignjob(input))

type master(name) =
    inherit Actor()
    
    override x.OnReceive message =
        match message with
        | :? string as msg -> sha256Hash msg k 
        | _-> failwith "unknown message"
        
let master =
    [1..1024]
    |> List.map(fun id -> let property = [| string(id) :> obj|]
                          system.ActorOf(Props(typedefof<master>, property)))
        
Async.RunSynchronously(boss,-1) |> ignore


