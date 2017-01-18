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

  member this.Array = data

  member this.pData =
    for i = 0 to data.Length-1 do
      if data.[i] = m1 then
        im1 <- i+1
      elif data.[i] = m2 then
        im2 <- i-1
    data <- data.[im1..im2]

    let array = data
                 |> List.map (fun e -> e.Split ([|" "|], System.StringSplitOptions.RemoveEmptyEntries))
                 |> List.map (Array.map decimal)

    //printfn "%s" "Hvor mange dage vil du have!??"
    //let n = System.Convert.ToInt32(System.Console.ReadLine());

    printfn "%A" array.[array.Length-n..array.Length-1]
end
;;

let dulle = new Loader("C:\Users\Bjarke\OneDrive\Datalogi\PoP\Projekter\11gPoP\11gData\data\Earth.txt", 5)
dulle.pData
