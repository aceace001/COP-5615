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
    | Work of int * int * int

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

let worker (mailbox:Actor<_>) = 
    let rec loop () = actor {
        let! msg = mailbox.Receive()
        match msg with
        | Work(starta, enda,input) -> 
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

            for items in names.[starta .. enda] do 
                let hash = sha256Hash(items)
                let count = leadzeros(hash) 
                if count = input then
                    printfn "%s %s" items hash
            
            mailbox.Sender() <! "Done"
            
        return! loop ()
    }
    loop ()

let worker1 = spawn system "Worker1" worker
let worker2 = spawn system "Worker2" worker
let worker3 = spawn system "Worker3" worker
let worker4 = spawn system "Worker4" worker
let worker5 = spawn system "Worker5" worker
let worker6 = spawn system "Worker6" worker
let worker7 = spawn system "Worker7" worker
let worker8 = spawn system "Worker8" worker

let master (mailbox:Actor<_>) =
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        let mutable nWorkLeft = 0

        match msg with
        |Assignjob(a)-> 
            let workuni = 3
            let input  = a
            let numactor = 8

            let worktotal = 95 * 95 * 95 

            let workeach = worktotal / numactor

            let start1: int = 1
            let start2: int = start1 + workeach
            let start3: int = start2 + workeach
            let start4: int = start3 + workeach
            let start5: int = start4 + workeach
            let start6: int = start5 + workeach
            let start7: int = start6 + workeach
            let start8: int = start7 + workeach
            
            worker1 <! Work(start1,start2-1, a)
            worker2 <! Work (start2,start3-1, a)
            worker3 <! Work (start3,start4-1,a)
            worker4 <! Work (start4,start5-1, a)
            worker5 <! Work (start5,start6-1,a)
            worker6 <! Work (start6,start7-1,a)
            worker7 <! Work (start7,start8-1, a)
            worker8 <! Work (start8,worktotal,a)
            
            mailbox.Sender() <! "Done"

        ///|Ans ->

           /// nWorkLeft <- nWorkLeft - 1
           /// if nWorkLeft = 0 then
           ///     Environment.Exit 1 
            

        return! loop()
    }
    loop()



let input = int (fsi.CommandLineArgs |> Seq.item 1)
let masteref = spawn system "master" master
masteref <! Assignjob(input)

system.Terminate()
