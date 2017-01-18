open System.Windows.Forms
open System.Drawing
open Graph

type pen = Color * float

/// Create a form and add a paint function
let createForm backgroundColor (width, height) title draw =
  let win = new Form ()
  win.Text <- title
  win.BackColor <- backgroundColor
  win.ClientSize <- Size (width, height)
  win.Paint.Add draw
  win

/// Draw a polygon with a specific color
let drawPoints (coords : polyline byref) (pen : pen) (e : PaintEventArgs) =
  let pairToPoint p =
    Point (int (round (fst p)), int (round (snd p)))
  let color, width = pen
  let Pen = new Pen (color, single width)
  let Points = Array.map pairToPoint (List.toArray coords)
  e.Graphics.DrawLines (Pen, Points)
  
/// Update the polygon
let updatePoints (form : Form) (coords : polyline byref) dtheta center showtime =
  let centeredCoords = translatePoints (scalePoint -1.0 center) coords
  let rotatedCoords = rotatePoints dtheta centeredCoords
  coords <- translatePoints center rotatedCoords
  form.Refresh ()

// Setup drawing details
let title = "A well organized triangle"
let backgroundColor = Color.White
let size = (400, 200)
let polarToCartesian r theta = (r * cos theta, r * sin theta)
let mutable coords = List.map (polarToCartesian 100.0) [0.0; 2.0/3.0*System.Math.PI; 4.0/3.0*System.Math.PI; 0.0]
let center = scalePoint (1.0/2.0) (float (fst size), float (snd size))
coords <- translatePoints center coords
let pen = (Color.Black, 1.0)
let win = createForm backgroundColor size title (drawPoints &coords pen) 

// Add timer events
let timer = new Timer()
timer.Interval <- 100
timer.Enabled <- true  
let dtheta = 0.01
timer.Tick.Add (updatePoints win &coords dtheta center)

// Start the event-loop.
Application.Run win
