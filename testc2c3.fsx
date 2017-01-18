open System
open  System.IO
open System.Windows.Forms
open System.Drawing
open Microsoft.FSharp.Control.CommonExtensions

let G = 67384e-11
let GMsol = 2.959122082322128e-4
let deltaT = 1.0
let AU = float(1495978707)
let deg2rad deg =
  deg * (System.Math.PI / 180.0)
type Position = {mutable x : float; mutable y : float; mutable z : float}
type Brush = Color * int
//type Point = float * float
//type Coordinate = {mutable x : float; mutable y : float;}
type planet (Lon0, Lat0, Rad0, Lon1, Lat1, Rad1) = class
      //startposition
      let x0 = Rad0 * cos(deg2rad Lat0) * cos(deg2rad Lon0)
      let y0 = Rad0 * cos(deg2rad Lat0) * sin(deg2rad Lon0)
      let z0 = Rad0 * sin(deg2rad Lat0)
      //gør posistionen om til en mutable
      let mutable _pos = {x = x0;
                          y = y0;
                          z = z0}
      //finder den næste position
      let x1 = Rad1 * cos(deg2rad Lat1) * cos(deg2rad Lon1)
      let y1 = Rad1 * cos(deg2rad Lat1) * sin(deg2rad Lon1)
      let z1 = Rad1 * sin(deg2rad Lat1)
      //dens startaccerelation. her vil solen(GMsol) trække lidt i planeten så den kører i en cirkel
      let ax0 = - GMsol / (Rad0 * Rad0 * Rad0) * x0
      let ay0 = - GMsol / (Rad0 * Rad0 * Rad0) * y0
      let az0 = - GMsol / (Rad0 * Rad0 * Rad0) * z0
      //hastigheden fundet ved at gange de to positioner
      let vx0 = (x1 - x0) / (deltaT - 0.0)
      let vy0 = (y1 - y0) / (deltaT - 0.0)
      let vz0 = (z1 - z0) / (deltaT - 0.0)
      //start positionen
      let rx0 = x0
      let ry0 = y0
      let rz0 = z0
      //ny position fundet med vores hastighed * deltaT(1)
      let rx1 = rx0 + vx0 * deltaT
      let ry1 = ry0 + vy0 * deltaT
      let rz1 = rz0 + vz0 * deltaT
      //den nye hastighed
      let vx1 = vx0 + ax0 * deltaT
      let vy1 = vy0 + ay0 * deltaT
      let vz1 = vz0 + az0 * deltaT
      //den nye acceleration
      let ax1 = - GMsol / (Rad1 * Rad1 * Rad1) * rx1
      let ay1 = - GMsol / (Rad1 * Rad1 * Rad1) * ry1
      let az1 = - GMsol / (Rad1 * Rad1 * Rad1) * rz1
      //gør værdierne mutable når jeg skal overskrive dem i min løkke
      let mutable rx = rx1 + vx1 * deltaT
      let mutable ry = ry1 + vy1 * deltaT
      let mutable rz = rz1 + vz1 * deltaT
      let mutable rsim = sqrt(rx1*rx1+ry1*ry1+rz1*rz1)
      let mutable ax = - GMsol / (rsim * rsim * rsim) * rx
      let mutable ay = - GMsol / (rsim * rsim * rsim) * ry
      let mutable az = - GMsol / (rsim * rsim * rsim) * rz
      let mutable vx = vx1 + ax1 * deltaT
      let mutable vy = vy1 + ay1 * deltaT
      let mutable vz = vz1 + az1 * deltaT
      //starter med dag = 0
      let mutable dag = 0.0
      //en tom liste til at indæstte planetens positioner
      let mutable _listpos = List.Empty
      //attributes
      member this.listpos = _listpos
      //methods
      //bevæger planeten
      member this.Move () =
          _pos <- {x = rx
                   y = ry
                   z = rz }
          ax <- - GMsol / (rsim * rsim * rsim) * rx
          ay <- - GMsol / (rsim * rsim * rsim) * ry
          az <- - GMsol / (rsim * rsim * rsim) * rz
          vx <- vx + ax * deltaT
          vy <- vy + ay * deltaT
          vz <- vz + az * deltaT
          rx <- rx + vx * deltaT
          ry <- ry + vy * deltaT
          rz <- rz + vz * deltaT
          rsim <- sqrt(rx*rx+ry*ry+rz*rz)
      //simulerer planeten med x antal dage
      member this.SimPlanet (dage:float) =
        while dag <= dage do
          _listpos <- _listpos @ [_pos]
          this.Move()
          dag <- dag + deltaT
    end
let earth = new planet(100.6001, 0.0044, 0.983295949009, 101.6192, 0.0044 ,0.983281670434)
let sun = new planet(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
let neptun = new planet(230.5130,1.7494,30.322768010002, 230.5189,1.7494,30.322769025525)
let venus = new planet(85.3426,0.5062,0.719928085839,86.9574,0.6007,0.719830023242)
let jupiter = new planet(87.8848,-0.2827,5.121965577530, 87.9704,-0.2808,5.122315061794)
let mars = new planet(326.0477,-1.8410,1.383211328675, 326.6812,-1.8386,1.382996396443)
let mercury = new planet(210.7567,2.1304,0.431506209573,213.9287,1.7561,0.435625389462)
let pluto = new planet(167.2515,14.4941,32.477005512413,167.2573 , 14.4950,32.476429022301)
let saturn = new planet(348.0949,-2.0195,9.642289048356, 348.1276 ,-2.0204,9.641988764325)
let uranus = new planet(167.2024,0.7721,18.285189312931, 167.2153 ,0.7721,18.285183922464)

sun.SimPlanet (365.0)
earth.SimPlanet (365.0)
neptun.SimPlanet (365.0)
venus.SimPlanet (365.0)
saturn.SimPlanet (365.0)
mars.SimPlanet (365.0)
mercury.SimPlanet (365.0)
jupiter.SimPlanet (365.0)
pluto.SimPlanet (365.0)
uranus.SimPlanet (365.0)
//printfn "%A" (earth.listpos)
//printfn "%A" (earth.listpos.[365])

;;
type Animation (listpos , listpos1, listpos2, listpos3,listpos4 , listpos5, listpos6, listpos7, listpos8, listpos9) = class
  let title = "Solarsystem"
  let backColor = Image.FromFile("Star_background.png")
  let size = (800, 600)
  let mutable positions = List.append listpos9 (List.append listpos8 (List.append listpos7 (List.append listpos6 (List.append listpos5 (List.append listpos4 (List.append listpos3 (List.append listpos2 (List.append listpos1 listpos))))))))
  let createForm backColor (width, height) title draw =
    let win = new Form ()
    win.Text <- title
    win.BackgroundImage <- backColor
    win.ClientSize <- Size (width, height)
    win.Paint.Add draw
    win
  let drawPlanet (e : PaintEventArgs) =
    let drawPixel (elm : Position) =
      match elm with
      | {Position.x=x;Position.y=y;} ->
        let rect = new Rectangle (int(x) + 400, int(y) + 200, 10, 10)
        let aBrush : SolidBrush = new SolidBrush (Color.Ivory)
        e.Graphics.FillEllipse (aBrush ,int(x), int(y), 2 ,2)
    List.map drawPixel positions |> ignore

  let AU2Pixel (AU:float ) =
    (float (fst size) / (2.0*33.0)) * AU + (float(fst size) / 2.0)
  member this.create () =
    let win = createForm backColor size title drawPlanet
    positions <-  positions |> List.map (fun {Position.x=x;Position.y=y;Position.z=z} -> {Position.x=AU2Pixel(x); Position.y=AU2Pixel(y);Position.z=AU2Pixel(z)})
    Application.EnableVisualStyles()
    Application.Run (win)
//let win = createForm backgroundColor size title drawPoints
end
;;
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

  member this.printData =
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

//let path = [{x=1.0;y=1.0;z=1.0;};{x=20.0;y=20.0;z=20.0;}]
let solardraw = new Animation (venus.listpos, neptun.listpos, earth.listpos, sun.listpos, mars.listpos, mercury.listpos, saturn.listpos, jupiter.listpos, pluto.listpos, uranus.listpos)
//let earthdraw = new Animation (earth.listpos)
//let neptundraw = new Animation (neptun.listpos)
solardraw.create()
//earthdraw.create()
//neptundraw.create()
