module client
open System
open Akka
open Akka.FSharp
open System.Collections.Generic
open MathNet.Numerics.Distributions 

let system = System.create "system" (Configuration.defaultConfig())

let tweetDB = new Dictionary<string, (String * list<string>) > ()

let relationDB = new Dictionary<string,list<string>>()

//let numberOfUser=Console.ReadLine()
let numberOfUser=2000

let normalDist = new Normal(50.0, 10.0) 

//let normalDist = new Normal((float(numberOfUser)/50.0), 10.0) 

(*
type Operation=
    | Register of username: string
    | Tweet of content: string 
    | Hashtag of tag: string 
    | Mention of mentions: string
    | Subscribe of username: string
    | Unsubscribe of username: string
    | Login of username: string
    | Logout of username: string
    | Query of username: string
    | Deliver of username: string
    | Print of username: string
*)

type Operation=
    | Register of string
    | Tweet of string 
    | Hashtag of string 
    | Mention of string
    | Subscribe of string
    | Unsubscribe of string
    | Login of string
    | Logout of string
    | Query of string
    | Deliver of string
    | Print of string
    
let rec remove n lst = 
    match lst with
    | h::tl when h = n -> tl
    | h::tl -> h :: (remove n tl)
    | []    -> []
 
(*   
//extract mentioned users
let extract_mention (str: string) =
    let equationPattern = "@(\w)+\s"
    let equationMatches = Regex.Matches(str, equationPattern)
    equationMatches
//extract hashtags    
let extract_hash (str: string) =
    let equationPattern = "#(\w)+\s"
    let equationMatches = Regex.Matches(str, equationPattern)
    equationMatches

*)
let register(username) =
    tweetDB.Add(username, ("Inactive",[]))
    relationDB.Add(username, [])

(*
combine login & logout
*)
(*
let login(name) =
    let temp=snd (tweetDB.Item(name))
    tweetDB.Item(name) <- ("Active", temp)
 
let logout(name) =
     let temp=snd (tweetDB.Item(name))
//     printfn("name: %A") name
     tweetDB.Item(name) <- ("Inactive", temp)
//     printfn("logout")
 
*)

(*
login and log out
*)
let loginNout(name)(status) =
    let mutable temp = snd (tweetDB.Item(name))
    if status = 1 then
        tweetDB.Item(name) <- ("Active", temp)
    elif status = 0 then
        tweetDB.Item(name) <- ("Inactive", temp)
        
let tweet(name) (message) =
//    let mutable temp=snd (tweetDB.Item(name))
//    temp <- temp @ [message]
    let temp = snd(tweetDB.Item(name)) @ [message]
    tweetDB.Item(name) <- ("Active", temp)

(*
combine sub and unsub
*)
(*
let subscribe(name) (username) =
     let mutable temp=relationDB.Item(name)
     temp <- temp @ [username]
     relationDB.Item(name) <- temp

let unsubscribe(name) (username) =
     let temp=remove username (relationDB.Item(name))
     relationDB.Item(name) <- temp
*)

let subNunsub(name)(username)(sub)=
    let mutable temp = relationDB.Item(name)
    if sub = 1 then      // subscribe
//        let mutable temp = relationDB.Item(name)
        temp <- temp @ [username]
    elif sub = 0 then    // unsubscribe 
        temp <- remove username (relationDB.Item(name))
    relationDB.Item(name) <- temp


(*
// combine hashtag and mention
let hashtag(tag: string) =
    for name in tweetDB.Keys do
        for each in (snd (tweetDB.Item(name)) ) do
            if (each.Contains (tag)) then
                printfn "%s" each
                printfn("each: %A") each
 
let mention (mentions:string) =
    for name in tweetDB.Keys do
        for each in (snd (tweetDB.Item(name)) ) do
            if (each.Contains mentions) then
               printfn "%s: %s" name each

*)
let tagNmention(content:string) =
    for name in tweetDB.Keys do
        for each in (snd (tweetDB.Item(name)) ) do
            if (each.Contains content) then
               if content.[0] = '#' then                // tag
                   printfn "%s" each
               if content.[0] = '@' then                // mention 
                   printfn "%s: %s" name each
let query(name) =
    for item in relationDB.Item(name) do
        for each in (snd (tweetDB.Item(item)) ) do
            printfn "%s: %s" item each   ///these could have problems, check later

let deliver(username) =
    printfn" "
    if (relationDB.ContainsKey username) then
        let watch_list = relationDB.[username]        
        for wtc in watch_list do
            for each1 in snd (tweetDB.[wtc]) do
                printfn"%s: %s" wtc each1
    for each2 in (snd (tweetDB.[username])) do
        printfn"%s: %s" username each2

let output() =
    printfn"*******************************************"
    printfn"Please select one of the following functions(only enter the number):"
    printfn"1.Tweet"
    printfn"2.My subscription"
    printfn"3.Hashtag query"
    printfn"4.Mention me"
    printfn"5.Subscribe"
    printfn"6.Unsubscribe"
    printfn"7.Retweet"
    printfn"8.Logout"
    printfn"*******************************************"
    
let Tweeter (mailbox : Actor<'a>) = //(name_obj : IActorRef)
    let name = mailbox.Self.Path.Name
    let rec loop (): Cont<Operation,'b> = actor {
        let! msg = mailbox.Receive()
        match msg with
        | Register username ->
            register(username)
        | Tweet message ->
            tweet(name)(message)
        | Login username ->
            loginNout(name)(1)
        | Subscribe username ->      
            subNunsub(name)(username)(1)
        | Unsubscribe username ->
            subNunsub(name)(username)(0)
        | Logout username ->
            loginNout(name)(0)
        | Hashtag tag ->
//            printfn("tag: %A") tag
            tagNmention(tag)
        | Mention mentions ->
//            printfn("mentions: %A") mentions 
            tagNmention(mentions)
        | Query username ->
            query(name)
        | Deliver username ->
            deliver(username)
        | Print username ->
            output()
        return! loop ()
      }
    loop ()
    


let ranUserName n = 
    let r = Random()
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|] ])
    let sz = Array.length chars in
    String(Array.init n (fun _ -> chars.[r.Next sz]))


let ranTweet n=
    let tag_num = Random().Next(2)
    let mention_num = Random().Next(2)
    let mutable str = ""
    let word_num = Random().Next(n)
    for i = 0 to word_num do
        str <- str + (ranUserName 10) + " " 
    for i = 0 to tag_num do
        str <- str + "#" + (ranUserName 5) + " "
    for i = 0 to mention_num do
        str <- str + "@" + (ranUserName 5) + " " 
    str
       
/// simulation starts

let actorArray = Array.create numberOfUser (Tweeter |> spawn system "tes1234")

{0..(numberOfUser-1)} |> Seq.iter (fun a ->
           //let m=(ranUserName 10)
           actorArray.[a] <- Tweeter |> spawn system (string a)
)
