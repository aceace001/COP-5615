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
let num_actor = 1
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

//// test ASCII_1 function 
////let randomEncoded = ascii_1("a")
////printfn("test ascii_1: %s\n") randomEncoded
//
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

// updated compute

let compute (numActor:int) (inpu:int) = 
    printfn("in compute")
    printfn("numActor: %A") numActor 
    let workunit:int = 95 * 95 *95 / numActor
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
        if count = inpu then
            printfn "%s %s" items hash
            
// new
let compute11 (num_actor: int)(inpu:int) =
    printfn("compute11")
    printfn ("~~~~inpu~~~~: %A~~~~~") inpu
    printfn("******num_actor*****: %A******\n") num_actor 
//    let workunit:int = 95 * 95 *95 / num_actor
    let substr = 3
    let start:char = ' '
    let startstring:string = string start
    let mutable workends:string = startstring
    let mutable lists : string list = []
    
    
    while (workends.Length <= substr) do
        let bitcoin:string = "yuhaoshi" + workends
        lists <- bitcoin :: lists      
        workends <- ascii_1(workends)
//        printfn("end while")
//    printfn("lists: %A") lists
//    printfn("num of lists: %A") lists.Length
//    printfn("item: %A")(lists.Item(1))
//    lists <- lists.[(num_actor - 1) * workunit..num_actor * workunit]
//    printfn("updated lists: %A") lists
    printfn("out of loop")   
    let names = lists |> List.toArray
    printfn("names.length: %A") names.Length
    ///let rand = new System.Random()
    ///let nl = names.[rand.Next(names.Length)]
    let workunit:int = 95 * 95 *95 / num_actor
    for items in names.[(num_actor - 1) * workunit .. num_actor * workunit] do 
        let hash = sha256Hash(items)
        let count = leadzeros(hash) 
        if count = inpu then
            printfn "%s %s" items hash
    printfn ("over!")

//compute11(1)(3)
// new

   
//compute 1
//let test (num:int) (sum:int) =
//    printfn("%A, %A") (num * 2) (sum)
//    
// using actor below
type master(name) =
    inherit Actor()
    
    override x.OnReceive message =
        match message with
        | :? int as msg -> compute11 msg k
//        | :? int as msg -> test msg 6
        | _-> failwith "unknown message"
 
   
let master =
    [1..num_actor]
    |> List.map(fun id -> let property = [| string(id) :> obj|]
                          system.ActorOf(Props(typedefof<master>, property)))




for id = 1 to num_actor do
    (rand.Next() % num_actor) |> List.nth master <! id

system.Terminate |> ignore 