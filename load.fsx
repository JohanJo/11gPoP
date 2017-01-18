open System
open System.IO

let filename = "C:\Users\Jebiel\Documents\Opgave 11g\data\Earth.txt"

let readLines (filename:string) = seq {
    use reader = new StreamReader (filename)
    while not reader.EndOfStream do
        yield reader.ReadLine ()
}

let tmp = readLines filename

let mutable data = Seq.toList tmp

let mutable im1 = 0
let mutable im2 = 0
let m1 = "$$SOE"
let m2 = "$$EOE"

for i = 0 to data.Length-1 do
  if data.[i] = m1 then
    im1 <- i+1
  elif data.[i] = m2 then
    im2 <- i-1

data <- data.[im1..im2]
//printfn "%A" data

//for i=0 to data.Length-1 do
//  noSpacesList.[i] <-  noSpacesList.[i] + data.[i]

let array = data
             |> List.map (fun e -> e.Split ([|" "|], System.StringSplitOptions.RemoveEmptyEntries))
             |> List.map (Array.map decimal)

printfn "%s" "Hvor mange dage vil du have!??"
let n = System.Convert.ToInt32(System.Console.ReadLine());

printfn "%A" array.[array.Length-n..array.Length-1]