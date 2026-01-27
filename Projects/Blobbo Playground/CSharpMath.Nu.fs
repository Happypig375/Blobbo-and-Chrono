namespace CSharpMath.Nu
open Nu
open System
open System.Collections.Generic
open System.Numerics
module [<AutoOpen>] Helpers =
    let (?^) (nullable: Nullable<'T>) (value: 'T) =
        if nullable.HasValue then nullable.GetValueOrDefault () else value
    let toNuColor (color: Drawing.Color) =
        Color (color.R, color.G, color.B, color.A)
    let toDrawingColor (color: Color) =
        Drawing.Color.FromArgb (int color.A8, int color.R8, int color.G8, int color.B8)

type NuCanvas (transform : Transform inref, world : Nu.World) =
    let mutable transform = transform
    member _.World = world
    member _.Transform = transform
    member val TransformStack = Stack<Transform> () with get
    member val CurrentColor = Nullable<Drawing.Color> () with get, set
    member val DefaultColor = Drawing.Color.Black with get, set
    member val CurrentStyle = CSharpMath.Rendering.FrontEnd.PaintStyle.Fill with get, set
    member this.Scale (sx, sy) = transform.Scale <- v3 (transform.Scale.X * sx) (transform.Scale.Y * sy) 1f
    interface CSharpMath.Rendering.FrontEnd.ICanvas with
        member this.Width = transform.Size.X * transform.Scale.X
        member this.Height = transform.Size.Y * transform.Scale.Y
        member this.CurrentColor with get () = this.CurrentColor and set value = this.CurrentColor <- value
        member this.DefaultColor with get () = this.DefaultColor and set value = this.DefaultColor <- value
        member this.CurrentStyle with get () = this.CurrentStyle and set value = this.CurrentStyle <- value
        member this.DrawLine (x1, y1, x2, y2, lineThickness) =
            World.renderVectorPath
                { Transform = transform
                  ClipOpt = ValueNone
                  Commands =
                    [| MoveTo (v2 x1 y1)
                       LineTo (v2 x2 y2) |]
                  FillColor = Color.Zero
                  WindingRule = WindingRule.Default
                  StrokeColor = this.CurrentColor ?^ this.DefaultColor |> toNuColor
                  StrokeThickness = lineThickness } world
        member this.FillRect (x, y, width, height) =
            World.renderVectorPath
                { Transform = transform
                  ClipOpt = ValueNone
                  Commands =
                    [| MoveTo (v2 x y)
                       LineTo (v2 (x + width) y)
                       LineTo (v2 (x + width) (y + height))
                       LineTo (v2 x (y + height))
                       CloseContour |]
                  FillColor = this.CurrentColor ?^ this.DefaultColor |> toNuColor
                  WindingRule = WindingRule.Default
                  StrokeColor = Color.Zero
                  StrokeThickness = 0.0f } world
        member this.StrokeRect (left, top, width, height) =
            World.renderVectorPath
                { Transform = transform
                  ClipOpt = ValueNone
                  Commands =
                    [| MoveTo (v2 left top)
                       LineTo (v2 (left + width) top)
                       LineTo (v2 (left + width) (top + height))
                       LineTo (v2 left (top + height))
                       CloseContour |]
                  FillColor = Color.Zero
                  WindingRule = WindingRule.Default
                  StrokeColor = this.CurrentColor ?^ this.DefaultColor |> toNuColor
                  StrokeThickness = 2f } world
        member this.Save () = this.TransformStack.Push transform
        member this.Restore () = transform <- this.TransformStack.Pop ()
        member this.Translate (dx, dy) = transform.Position <- transform.Position + (v3 dx dy 0f * transform.Scale * transform.Size).Transform transform.Rotation
        member this.Scale (sx, sy) = this.Scale (sx, sy)
        member this.StartNewPath () = new NuPath (this)

and NuPath (owner : NuCanvas) =
    inherit CSharpMath.Rendering.FrontEnd.Path ()
    member val VectorPathCommands = ResizeArray<VectorPathCommand>()
    override this.MoveTo(x, y) = this.VectorPathCommands.Add (MoveTo (v2 x y))
    override this.LineTo(x, y) = this.VectorPathCommands.Add (LineTo (v2 x y))
    override this.Curve3(cpx, cpy, x, y) = this.VectorPathCommands.Add (QuadraticCurveTo (v2 cpx cpy, v2 x y))
    override this.Curve4(cp1x, cp1y, cp2x, cp2y, x, y) =
        this.VectorPathCommands.Add (CubicCurveTo (v2 cp1x cp1y, v2 cp2x cp2y, v2 x y))
    override this.CloseContour() = this.VectorPathCommands.Add CloseContour
    override val Foreground = Nullable<Drawing.Color>() with get, set
    override this.Dispose () =
        let color = this.Foreground ?^ owner.CurrentColor ?^ owner.DefaultColor |> toNuColor
        World.renderVectorPath
            { Transform = owner.Transform
              ClipOpt = ValueNone
              Commands = this.VectorPathCommands.ToArray()
              FillColor = if owner.CurrentStyle = CSharpMath.Rendering.FrontEnd.PaintStyle.Fill then color else Color.Zero
              WindingRule = WindingRule.Default
              StrokeColor = if owner.CurrentStyle = CSharpMath.Rendering.FrontEnd.PaintStyle.Stroke then color else Color.Zero
              StrokeThickness = 2.0f } owner.World

type MathPainter () =
    inherit CSharpMath.Rendering.FrontEnd.MathPainter<NuCanvas, Color> ()
    override _.WrapCanvas canvas = canvas
    override _.UnwrapColor color = color |> toNuColor
    override _.WrapColor color = color |> toDrawingColor

namespace BlobboPlayground
open Nu
open CSharpMath.Nu
open CSharpMath.Rendering.FrontEnd
open System.Numerics
module [<AutoOpen>] MathFacetExtensions =
    type Entity with
        member this.GetLaTeX world : string = this.Get (nameof this.LaTeX) world
        member this.SetLaTeX (value : string) world = this.Set (nameof this.LaTeX) value world
        member this.LaTeX = lens (nameof this.LaTeX) this this.GetLaTeX this.SetLaTeX
        member this.GetMathFontSize world : single = this.Get (nameof this.MathFontSize) world
        member this.SetMathFontSize (value : single) world = this.Set (nameof this.MathFontSize) value world
        member this.MathFontSize = lens (nameof this.MathFontSize) this this.GetMathFontSize this.SetMathFontSize
        member this.GetMathPainter world : MathPainter = this.Get (nameof this.MathPainter) world
        member this.MathPainter = lensReadOnly (nameof this.MathPainter) this this.GetMathPainter

type MathFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.LaTeX ""
         define Entity.MathFontSize 12f
         define Entity.Color Color.White
         nonPersistent Entity.MathPainter (MathPainter ())]

    override this.Register (entity, world) =
        World.sense (fun event world ->
            let painter = event.Subscriber.GetMathPainter world
            painter.LaTeX <- (event.Data : ChangeData).Value :?> string
            let size = painter.Measure 0f
            event.Subscriber.SetSize (v3 size.Width size.Height 0f) world
            Cascade) entity.LaTeX.ChangeEvent entity (nameof MathFacet) world
        World.sense (fun event world ->
            let painter = event.Subscriber.GetMathPainter world
            painter.FontSize <- (event.Data : ChangeData).Value :?> single
            let size = painter.Measure 0f
            event.Subscriber.SetSize (v3 size.Width size.Height 0f) world
            Cascade) entity.MathFontSize.ChangeEvent entity (nameof MathFacet) world
        World.sense (fun event world ->
            (event.Subscriber.GetMathPainter world).TextColor <- (event.Data : ChangeData).Value :?> Color
            Cascade) entity.Color.ChangeEvent entity (nameof MathFacet) world

    override this.Render (_, entity, world) =
        let painter = entity.GetMathPainter world
        let height = painter.Display.Ascent + painter.Display.Descent
        if painter.Display.Width > 0f && height > 0f then
            let transform = entity.GetTransform world
            let canvas = NuCanvas (&transform, world)
            canvas.Scale (1f / painter.Display.Width, -1f / height)
            painter.Draw (canvas, painter.Display.Width * -0.5f, (painter.Display.Ascent - painter.Display.Descent) * 0.5f)