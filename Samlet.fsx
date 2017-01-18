open System
open System.IO
open System.Windows.Forms
open System.Drawing
open Microsoft.FSharp.Control.CommonExtensions

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

    array <- array.[array.Length-n..array.Length-1]

  member this.lon0 = (Array.get array.[0] 1) |> float
  member this.lon1 = (Array.get array.[1] 1) |> float
  member this.lat0 = (Array.get array.[0] 2) |> float
  member this.lat1 = (Array.get array.[1] 2) |> float
  member this.rad0 = (Array.get array.[0] 3) |> float
  member this.rad1 = (Array.get array.[1] 3) |> float
end

// load filerne med positionsdata
let mercuryData = new Loader("data/Mercury.txt", 5)
let venusData = new Loader("data/Venus.txt", 5)
let earthData = new Loader("data/Earth.txt", 5)
let marsData = new Loader("data/Mars.txt", 5)
let saturnData = new Loader("data/Saturn.txt", 5)
let uranusData = new Loader("data/Uranus.txt", 5)
let jupiterData = new Loader("data/Jupiter.txt", 5)
let neptuneData = new Loader("data/Neptune.txt", 5)
let plutoData = new Loader("data/Pluto.txt", 5)


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

let earth = new planet(earthData.lon0, earthData.lat0, earthData.rad0, earthData.lon1, earthData.lat1 ,earthData.rad1)
let sun = new planet(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
let neptun = new planet(230.5130,1.7494,30.322768010002, 230.5189,1.7494,30.322769025525)
let venus = new planet(85.3426,0.5062,0.719928085839,86.9574,0.6007,0.719830023242)
//sun.SimPlanet (1000.0)
earth.SimPlanet (365.0)
neptun.SimPlanet (365.0)
venus.SimPlanet (365.0)


;;
type Animation (listpos) = class
  //inherit planet (Lon0, Lat0, Rad0, Lon1, Lat1, Rad1)
  let title = "Solarsystem"
  let backColor = Color.White
  let size = (800, 400)
  let mutable positions = listpos
  let createForm backColor (width, height) title draw =
    let win = new Form ()
    win.Text <- title
    win.BackColor <- backColor
    win.ClientSize <- Size (width, height)
    win.Paint.Add draw
    win
  let drawPlanet (e : PaintEventArgs) =
    let drawPixel (elm : Position) =
      match elm with
      | {Position.x=x;Position.y=y;} ->
        let rect = new Rectangle (int(x) + 400, int(y) + 200, 10, 10)
        let aBrush : SolidBrush = new SolidBrush (Color.Blue)
        e.Graphics.FillEllipse (aBrush ,int(x), int(y), 5 ,5)
    List.map drawPixel positions |> ignore

  let AU2Pixel (AU:float ) =
    (float (fst size) / (2.0*33.0)) * AU + (float(fst size) / 2.0)
  member this.create () =
    let win = createForm backColor size title drawPlanet
    positions <-  listpos |> List.map (fun {Position.x=x;Position.y=y;Position.z=z} -> {Position.x=AU2Pixel(x); Position.y=AU2Pixel(y);Position.z=AU2Pixel(z)})
    Application.EnableVisualStyles ()
    Application.Run (win)

//let win = createForm backgroundColor size title drawPoints
end
//let path = [{x=1.0;y=1.0;z=1.0;};{x=20.0;y=20.0;z=20.0;}]
let venusdraw = new Animation (venus.listpos)
//let earthdraw = new Animation (earth.listpos)
//let neptundraw = new Animation (earth.listpos)

venusdraw.create()
//earthdraw.create()
//neptundraw.create()
