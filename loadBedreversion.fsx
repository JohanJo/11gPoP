open System
open System.IO

type Loader (filename:string, n:int)= class
  let readLines (filename:string) = seq {
      use reader = new StreamReader (filename)
      while not reader.EndOfStream do
          yield reader.ReadLine ()
  }

  let mutable im1 = 0
  let mutable im2 = 0
  let tmp = readLines filename
  let mutable data = Seq.toList tmp
  let m1 = "$$SOE"
  let m2 = "$$EOE"
  let mutable array = []

  member this.Array = array

  member this.pData =
    for i = 0 to data.Length-1 do
      if data.[i] = m1 then
        im1 <- i+1
      elif data.[i] = m2 then
        im2 <- i-1
    data <- data.[im1..im2]
    array <- data |> List.map (fun e -> e.Split ([|" "|], System.StringSplitOptions.RemoveEmptyEntries))
    //|> List.map (Array.map decimal)
    //printfn "%s" "Hvor mange dage vil du have!??"
    //let n = System.Convert.ToInt32(System.Console.ReadLine());

    array <- array.[array.Length-n..array.Length-1]
    //(Array.get array.[0] 1)
    
  member this.lon0 = (Array.get array.[0] 1) |> decimal
  member this.lon1 = (Array.get array.[1] 1) |> decimal
  member this.lat0 = (Array.get array.[0] 2) |> decimal
  member this.lat1 = (Array.get array.[1] 2) |> decimal
  member this.rad0 = (Array.get array.[0] 3) |> decimal
  member this.rad1 = (Array.get array.[1] 3) |> decimal
end

let dulle = new Loader("C:\Users\Jebiel\Documents\Opgave 11g\data\Earth.txt", 5)
let mulle = new Loader("C:\Users\Jebiel\Documents\Opgave 11g\data\Venus.txt", 5)
dulle.pData
printfn "%A" dulle.Array
printf "%A " dulle.lon0
printf "%A " dulle.lon1
printf "%A " dulle.lat0
printf "%A " dulle.lat0
printf "%A " dulle.rad0
printf "%A " dulle.rad1

