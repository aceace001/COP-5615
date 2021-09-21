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

let rec ascii (s : string) = 
    
    let start:char = ' '
    let ends:char = '~'
    
    let startstring:string = string start
    let endstring:string = string ends
    let isCorrect = String.forall (fun c -> c = ends)

    let ss:char = s.[s.Length - 1]
   
    if (ss = ends) then
        ascii(s.[0..s.Length-2]) + startstring
    else
        let sss:int = int ss + 1
        s.[0..(s.Length - 2)] + string (Convert.ToChar(sss))


let ascii_1 (s : string) = 
    
    let start:char = ' '
    let ends:char = '~'
    
    let startstring:string = string start

    let isCorrect = String.forall (fun c -> c = ends)

    let ss:char = s.[s.Length - 1]
   
    if (isCorrect(s)) then
        String.replicate (s.Length + 1) startstring
    else 
        ascii(s)

///let aaa = ascii_1("a~~")

let rec leadzeros (h:string) = 
    if h.[0] <> '0' then
        0
    else 
        1 + leadzeros(h.[1..h.Length])

let compute inpu = 

    let workunit:int = 3
    let start:char = ' '
    let startstring:string = string start
    let mutable workends:string = startstring
    let mutable lists : string list = []
    

    while (workends.Length <= workunit) do
        let bitcoin:string = "yuhaoshi" + workends
        lists <- bitcoin :: lists
        
        workends <- ascii_1(workends)
        
    let names = lists |> List.toArray
    ///let rand = new System.Random()
    ///let nl = names.[rand.Next(names.Length)]

    for items in names.[0 .. 10000] do 
        let hash = sha256Hash(items)
        let count = leadzeros(hash) 
        if (int64(count) = input) then
            printfn "%s %s" nl hash

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


let master = spawn system "master" <| fun mailbox ->
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        let mutable numactor = 0

        match msg with
        |Assignjob(a)-> let re = splitwork a
                        for r in re do
                            Async.RunSynchronously(r,-1) |> ignore
                        mailbox.Sender() <! "Done"

        return! loop()
    }
    loop()


let input:int = int fsi.CommandLineArgs.[0]
let boss = (master <? Assignjob(input))

Async.RunSynchronously(boss,-1) |> ignore

