open Suave
open Suave.Http
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open Suave.Utils
open System
open System.Net
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Newtonsoft.Json
open FSharp.Json
open System.Collections.Generic



let userDB = new Dictionary<string, string>()
let tDB = new Dictionary<string, (string * list<string>) >()
let rDB = new Dictionary<string, list<string>>()

type tweettypes = 
    { msgtp: string
      name : string
      content : string }
    
let ws (webSocket : WebSocket) (context: HttpContext)=
    socket {
        let mutable loop = true
        while loop do
            let! msg = webSocket.read()
            match msg with
            | (Text, data, true) ->
                let str = UTF8.toString data
                let tweet_msg = Json.deserialize<tweettypes> str
                let mType=tweet_msg.msgtp
                
                if mType="tweet" then
                    let temp= snd (tDB.Item(tweet_msg.name)) @ [tweet_msg.content]
                    tDB.Item(tweet_msg.name) <- ("Active", temp)
                    let response1:tweettypes = {
                        msgtp= "tweet"
                        name = tweet_msg.name
                        content = "Your tweet is:"
                    }
                    let json1 = Json.serialize response1
                    let byteResponse1 =
                      json1
                      |> System.Text.Encoding.ASCII.GetBytes
                      |> ByteSegment
                    do! webSocket.send Text byteResponse1 true
                    let response:tweettypes = {
                        msgtp= "tweet"
                        name = tweet_msg.name
                        content = tweet_msg.name + " : " + tweet_msg.content
                    }
                    let json = Json.serialize response
                    let byteResponse =
                      json
                      |> System.Text.Encoding.ASCII.GetBytes
                      |> ByteSegment
                    do! webSocket.send Text byteResponse true
                                                                                                              
                    printfn "uploaded into the database from user %s" tweet_msg.name
                elif mType="register" then
                    if tDB.ContainsKey tweet_msg.name then
                        printfn "user register failed: duplicated name"
                        let response:tweettypes = {
                            msgtp = "register";
                            name = tweet_msg.name;
                            content = "user already registered, please try a different name"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true
                    else
                        tDB.Add(tweet_msg.name, ("Inactive",[]))
                        printfn "user %s registered" tweet_msg.name
                        rDB.Add(tweet_msg.name, [])
                        userDB.Add(tweet_msg.name, tweet_msg.content)

                        let response:tweettypes = {
                            msgtp = "register";
                            name = tweet_msg.name;
                            content = "user " + tweet_msg.name + " registered"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true

                elif mType="login" then
                    if tDB.ContainsKey tweet_msg.name then
                        if tweet_msg.content = userDB.[tweet_msg.name] then
                            let response:tweettypes = {
                                msgtp = "login";
                                name = tweet_msg.name;
                                content = "user " + tweet_msg.name + " logged in"
                            }
                            let json = Json.serialize response
                            let byteResponse =
                              json
                              |> System.Text.Encoding.ASCII.GetBytes
                              |> ByteSegment
                            do! webSocket.send Text byteResponse true

                            let temp=snd (tDB.Item(tweet_msg.name))
                            tDB.Item(tweet_msg.name) <- ("Active", temp)
                            printfn "user %s login successfully" tweet_msg.name
                        else
                            printfn "login failed: incorrect username or password"
                            let response: tweettypes = {
                                msgtp= "login"
                                name = "NA"
                                content = "incorrect username or password"
                            }
                            let json = Json.serialize response
                            let byteResponse =
                              json
                              |> System.Text.Encoding.ASCII.GetBytes
                              |> ByteSegment
                            do! webSocket.send Text byteResponse true
                    else
                        printfn "login failed: no user found"
                        let response:tweettypes = {
                            msgtp = "login";
                            name = "Error";
                            content = "user does not exist, try again"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true
                elif mType= "subscribe" then
                    if tDB.ContainsKey tweet_msg.name then 
                        let temp= rDB.Item(tweet_msg.name) @ [tweet_msg.content]
                        rDB.Item(tweet_msg.name) <- temp
                        printfn "user %s subscribed" tweet_msg.content

                    else 
                        printfn "subscribe failed: no user name found"
                        let response:tweettypes = {
                            msgtp = "subscribe";
                            name = "error";
                            content = "subsribe failed: no user " + tweet_msg.name + " found"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true
                elif mType= "retweet" then
                    printfn "user %s retweet" tweet_msg.name
                    
                    let response1:tweettypes = {
                        msgtp= "retweet"
                        name = tweet_msg.name
                        content = "Retweet from user " + tweet_msg.name
                    }
                    let json1 = Json.serialize response1
                    let byteResponse1 =
                        json1
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse1 true

                    for i in snd (tDB.[tweet_msg.name]) do
                        let response:tweettypes= {
                            msgtp = "retweet"
                            name = tweet_msg.name
                            content = i 
                        }
                        let json = Json.serialize response
                        let byteResponse =
                            json
                            |> System.Text.Encoding.ASCII.GetBytes
                            |> ByteSegment
                        do! webSocket.send Text byteResponse true
                        printfn"%s" i
                elif mType= "display" then
                    printfn "user %s displayed subscriptions and tweets" tweet_msg.name
                    
                    let response1:tweettypes = {
                        msgtp= "display"
                        name = tweet_msg.name
                        content = "Your subscriptions with their tweets and yours are:"
                    }
                    let json1 = Json.serialize response1
                    let byteResponse1 =
                        json1
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse1 true

                    if rDB.[tweet_msg.name] <> [] then 
                        let sbs = rDB.[tweet_msg.name]        
                        for i in sbs do
                            for j in snd (tDB.[i]) do
                                let response:tweettypes= {
                                    msgtp = "display"
                                    name = tweet_msg.name
                                    content = i + " : " + j 
                                }
                                let json = Json.serialize response
                                let byteResponse =
                                    json
                                    |> System.Text.Encoding.ASCII.GetBytes
                                    |> ByteSegment
                                do! webSocket.send Text byteResponse true
                                printfn"%s: %s" i j
                    for k in (snd (tDB.[tweet_msg.name])) do
                        let response:tweettypes = {
                            msgtp= "display"
                            name = tweet_msg.name
                            content = tweet_msg.name + " : " + k
                        }
                        let json = Json.serialize response
                        let byteResponse =
                            json
                            |> System.Text.Encoding.ASCII.GetBytes
                            |> ByteSegment
                        do! webSocket.send Text byteResponse true
                        printfn"%s: %s" tweet_msg.name k
                    
                elif mType= "search" then
                    let response1:tweettypes= {
                        msgtp= "search"
                        name = tweet_msg.name
                        content = "Your Search Results:"
                    }
                    let json1 = Json.serialize response1
                    let byteResponse1 =
                      json1
                      |> System.Text.Encoding.ASCII.GetBytes
                      |> ByteSegment
                    do! webSocket.send Text byteResponse1 true

                    for name in tDB.Keys do
                        for each in (snd (tDB.Item(name)) ) do
                            if (each.Contains tweet_msg.content) then 
                                let response:tweettypes= {
                                    msgtp= "search"
                                    name = tweet_msg.name
                                    content = name + " : " + each
                                }
                                let json = Json.serialize response
                                let byteResponse =
                                  json
                                  |> System.Text.Encoding.ASCII.GetBytes
                                  |> ByteSegment
                                do! webSocket.send Text byteResponse true
                                printfn "%s" each
                    printfn "search %s successfully" (tweet_msg.content)                   
            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
      }      

let app : WebPart =
    choose [
        path "/websocket" >=> handShake ws
        GET >=> choose [ path "/" >=> file "index.html"; browseHome ]
        NOT_FOUND "Found no handlers." ]

[<EntryPoint>]
let main _ =
  startWebServer { defaultConfig with logger = Targets.create Verbose [||] } app
  0
