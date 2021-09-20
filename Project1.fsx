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

// input number of leading 0's we need 
let k:int = int (fsi.CommandLineArgs |> Seq.item 1)
let num_actor = 1024
let rand = Random(1234)
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

// test sha256Hash function 
//let encoded = sha256Hash("dafei.du")
//printfn ("test sha256 hash: %s\n") encoded

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

// test ASCII_1 function 
//let randomEncoded = ascii_1("a")
//printfn("test ascii_1: %s\n") randomEncoded

let rec leadzeros (h:string) = 
    if h.[0] <> '0' then
        0
    else 
        1 + leadzeros(h.[1..h.Length])


//let k = fsi.CommandLineArgs |> Seq.item 1
//
//printfn ("%A\n") k
//let res = leadzeros(k)
//printfn ("test leading zeros: %A\n") res

let compute inpu = 
    let workunit:int = 10
    let start:char = ' '
    let startstring:string = string start
    let mutable workends:string = startstring

    while (workends.Length <= workunit) do
        let bitcoin:string = "yuhaoshi" + workends
        let hash = sha256Hash(bitcoin)
        let count = leadzeros(hash) 
        ///Seq.length(Seq.filter (fun x' -> x' = '0') hash.[0..workunit])
//        printfn ("%s\n") workends 
        if (count = inpu) then
            printfn "%s %s" bitcoin hash

        workends <- ascii_1(workends)


//compute k

// using actor below
type master(name) =
    inherit Actor()
    
    override x.OnReceive message =
        match message with
        | :? int as k -> compute k
        | _-> failwith "unknown message"
 
   
let master =
    [1..num_actor]
    |> List.map(fun id -> let property = [| string(id) :> obj|]
                          system.ActorOf(Props(typedefof<master>, property)))




for id = 1 to num_actor do
    (rand.Next() % num_actor) |> List.nth master <! id

system.Terminate |> ignore 