module server

open System
open Akka
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit
open System.Threading
open client

let stopWatch=System.Diagnostics.Stopwatch.StartNew()


type message2=
    | Simulate of username: string
    | LoginLive of username: string 
    | Report of username: string
    | TweetLive of username: string
    | ReTweetLive of username: string
    | FollowerRe of username: string
    
Thread.Sleep(500)   
let Simulator (mailbox : Actor<'a>) = //(name_obj : IActorRef)
    let name = mailbox.Self.Path.Name
    let rec loop (): Cont<message2,'b> = actor {
            let! msg = mailbox.Receive()
            match msg with
            | Simulate username->
                for i=0 to (numberOfUser-1) do
                    tweetDB.Add((string i), ("Inactive",[]))
                    relationDB.Add((string i), [])
            | LoginLive username->
                let keys=tweetDB.Keys
                // live login and logout
                {0..(numberOfUser-1)} |> Seq.iter (fun a ->
                         let rnd=new Random()
                         let n=rnd.Next(numberOfUser)
                         if(n%3<>0) 
                         then actorArray.[n] <! Login "1"
                         else actorArray.[n] <! Logout "1"
                )
            | TweetLive username->
                //live tweeting
                {0..(numberOfUser-1)} |> Seq.iter (fun a ->
                         let temp = actorArray.[a].Path.Name
                         let check= fst (tweetDB.Item(temp))
                         if(check="Active")
                         then actorArray.[a] <! Tweet (ranTweet 2)
                )
            | Report username-> 
                //report the output
                let mutable numOfActive=0
                let mutable numOfInactive=0
                let mutable numOfTweet=0
                let mutable numOfSubscribe=0
                {0..(numberOfUser-1)} |> Seq.iter (fun a ->
                         let temp = actorArray.[a].Path.Name
                         let check= fst (tweetDB.Item(temp))
                         if(check="Active")
                         then numOfActive <- numOfActive+1
                         else numOfInactive<- numOfInactive+1
                         let tweets= snd (tweetDB.Item(temp))
                         numOfTweet <- numOfTweet+tweets.Length
                         let subscribers = relationDB.Item(temp)
                         numOfSubscribe <- numOfSubscribe+subscribers.Length
                )    
                printfn "Total number of users: %i " numberOfUser
                printfn "Total number of online users: %i " numOfActive
                printfn "Total number of offline users: %i " numOfInactive
                printfn "Total number of tweets: %i " numOfTweet
                printfn "Total number of total subscribing: %i " numOfSubscribe
            | ReTweetLive username ->
                let mutable num_sub = normalDist.Sample() 
                let mutable num_sub_int = int(num_sub)
                let mutable new_num_twe = int(num_sub/5.0)
                let rnd=new Random()
                {0..(numberOfUser-1)} |> Seq.iter (fun a ->
                    num_sub <- normalDist.Sample() 
                    num_sub_int <- int(num_sub)
                    new_num_twe <- int(num_sub/5.0)
                    let the_user = actorArray.[a].Path.Name
                    let mutable his_sub = relationDB.[the_user]                    

                    if new_num_twe > 0 then
                        for num = 1 to new_num_twe do
                            actorArray.[a] <! Tweet (ranTweet 2)

                    let his_tweet = snd (tweetDB.[the_user])

                    while(num_sub_int>0) do
                        let mutable n=rnd.Next(numberOfUser)
                        let mutable the_sub = actorArray.[n].Path.Name
                        while(((List.exists (fun elem -> elem = the_sub) his_sub) = true) || the_sub=the_user) do
                            n <- rnd.Next(numberOfUser)
                            the_sub <- actorArray.[n].Path.Name
                        his_sub <- his_sub @ [the_sub]
                        
                        (*
                        //if (his_tweet.Length > 0) then
                        let m = rnd.Next(his_tweet.Length)
                        let new_tweet = the_user + ": " + his_tweet.[m]

                        let temp=snd (tweetDB.Item(the_sub))
                        let temp=temp @ [new_tweet]
                        tweetDB.Item(the_sub) <- ("Active", temp)

                        //actorArray.[n] <! Tweet new_tweet
                        *)

                        num_sub_int <- num_sub_int - 1

                    relationDB.Item(the_user) <- his_sub
                )
            | FollowerRe username ->
                for item in relationDB.Keys do
                    let followers = relationDB.[item]
                    let tweets = snd (tweetDB.[item])
                    for fllw in followers do
                        let rnd=new Random()
                        //if (his_tweet.Length > 0) then
                        let m = rnd.Next(tweets.Length)
                        let new_tweet = item + ": " + tweets.[m]
                        let temp=snd (tweetDB.[fllw])
                        let temp=temp @ [new_tweet]
                        tweetDB.Item(fllw) <- ("Active", temp)                                        
                //actorArray.[n] <! Tweet new_tweet 
          }
    loop ()
