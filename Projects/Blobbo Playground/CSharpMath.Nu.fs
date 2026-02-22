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

type NuCanvas (width, height, strokeThickness) =
    let transformStack = Stack<Matrix3x2> ()
    let mutable _transform = Matrix3x2.Identity
    let mutable _scale = v2One
    member this.Transform
        with get () = _transform
        and set value =
            _transform <- value
            let scaleX = sqrt (this.Transform.M11 * this.Transform.M11 + this.Transform.M12 * this.Transform.M12)
            let scaleY = sqrt (this.Transform.M21 * this.Transform.M21 + this.Transform.M22 * this.Transform.M22)
            _scale <- v2 (scaleX * width) (scaleY * height)
    member _.CurrentScale = _scale
    member val Tessellations = List<ContourTessellation * Matrix3x2> () with get
    member val CurrentColor = Nullable<Drawing.Color> () with get, set
    member val DefaultColor = Drawing.Color.Black with get, set
    member val CurrentStyle = CSharpMath.Rendering.FrontEnd.PaintStyle.Fill with get, set
    member this.Scale (sx : single, sy : single) =
        this.Transform <- Matrix3x2.CreateScale (sx, sy) * this.Transform
    interface CSharpMath.Rendering.FrontEnd.ICanvas with
        member this.Width = width
        member this.Height = height
        member this.CurrentColor with get () = this.CurrentColor and set value = this.CurrentColor <- value
        member this.DefaultColor with get () = this.DefaultColor and set value = this.DefaultColor <- value
        member this.CurrentStyle with get () = this.CurrentStyle and set value = this.CurrentStyle <- value
        member this.DrawLine (x1, y1, x2, y2, lineThickness) =
            this.Tessellations.Add
                (ContourTessellation.make
                    [| MoveTo (v2 x1 y1)
                       LineTo (v2 x2 y2) |]
                    ContourFill.none
                    (ContourStroke.antiAliased (this.CurrentColor ?^ this.DefaultColor |> toNuColor) lineThickness)
                    this.CurrentScale, this.Transform)
        member this.FillRect (x, y, width, height) =
            this.Tessellations.Add
                (ContourTessellation.make
                    [| MoveTo (v2 x y)
                       LineTo (v2 (x + width) y)
                       LineTo (v2 (x + width) (y + height))
                       LineTo (v2 x (y + height))
                       CloseContour |]
                    (ContourFill.ofColor (this.CurrentColor ?^ this.DefaultColor |> toNuColor))
                    ContourStroke.none
                    this.CurrentScale, this.Transform)
        member this.StrokeRect (x, y, width, height) =
            this.Tessellations.Add
                (ContourTessellation.make
                    [| MoveTo (v2 x y)
                       LineTo (v2 (x + width) y)
                       LineTo (v2 (x + width) (y + height))
                       LineTo (v2 x (y + height))
                       CloseContour |]
                    ContourFill.none
                    (ContourStroke.antiAliased (this.CurrentColor ?^ this.DefaultColor |> toNuColor) strokeThickness)
                    this.CurrentScale, this.Transform)
        member this.Save () = transformStack.Push this.Transform
        member this.Restore () = this.Transform <- transformStack.Pop ()
        member this.Translate (dx, dy) = this.Transform <- Matrix3x2.CreateTranslation (v2 dx dy) * this.Transform
        member this.Scale (sx, sy) = this.Scale (sx, sy)
        member this.StartNewPath () = new NuPath (this, strokeThickness)

and NuPath (owner : NuCanvas, strokeThickness : single) =
    inherit CSharpMath.Rendering.FrontEnd.Path ()
    member val ContourCommands = List<ContourCommand> ()
    override this.MoveTo (x, y) = this.ContourCommands.Add (MoveTo (v2 x y))
    override this.LineTo (x, y) = this.ContourCommands.Add (LineTo (v2 x y))
    override this.Curve3 (cpx, cpy, x, y) = this.ContourCommands.Add (QuadraticCurveTo (v2 cpx cpy, v2 x y))
    override this.Curve4 (cp1x, cp1y, cp2x, cp2y, x, y) =
        this.ContourCommands.Add (CubicCurveTo (v2 cp1x cp1y, v2 cp2x cp2y, v2 x y))
    override this.CloseContour () = this.ContourCommands.Add CloseContour
    override val Foreground = Nullable<Drawing.Color> () with get, set
    override this.Dispose () =
        let color = this.Foreground ?^ owner.CurrentColor ?^ owner.DefaultColor |> toNuColor
        owner.Tessellations.Add
            (ContourTessellation.make
                this.ContourCommands
                (ContourFill.ofColor (if owner.CurrentStyle = CSharpMath.Rendering.FrontEnd.PaintStyle.Fill then color else Color.Zero))
                (ContourStroke.antiAliased (if owner.CurrentStyle = CSharpMath.Rendering.FrontEnd.PaintStyle.Stroke then color else Color.Zero) strokeThickness)
                owner.CurrentScale, owner.Transform)

type MathPainter () =
    inherit CSharpMath.Rendering.FrontEnd.MathPainter<NuCanvas, Color> ()
    override _.WrapCanvas canvas = canvas
    override _.UnwrapColor color = color |> toNuColor
    override _.WrapColor color = color |> toDrawingColor

namespace BlobboPlayground
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open CSharpMath.Atom
open CSharpMath.Editor
open CSharpMath.Nu
module [<AutoOpen>] MathFacetExtensions =
    type Entity with
        member this.GetTessellations world : (ContourTessellation * Matrix3x2) List = this.Get (nameof this.Tessellations) world
        member this.SetTessellations (value : (ContourTessellation * Matrix3x2) List) world = this.Set (nameof this.Tessellations) value world
        member this.Tessellations = lens (nameof this.Tessellations) this this.GetTessellations this.SetTessellations
        member this.GetLaTeX world : string = this.Get (nameof this.LaTeX) world
        member this.SetLaTeX (value : string) world = this.Set (nameof this.LaTeX) value world
        member this.LaTeX = lens (nameof this.LaTeX) this this.GetLaTeX this.SetLaTeX
        member this.GetMathFontSize world : single = this.Get (nameof this.MathFontSize) world
        member this.SetMathFontSize (value : single) world = this.Set (nameof this.MathFontSize) value world
        member this.MathFontSize = lens (nameof this.MathFontSize) this this.GetMathFontSize this.SetMathFontSize
        member this.GetMathNextInsertion world : MathListIndex option = this.Get (nameof this.MathNextInsertion) world
        member this.SetMathNextInsertion (value : MathListIndex option) world = this.Set (nameof this.MathNextInsertion) value world
        member this.MathNextInsertion = lens (nameof this.MathNextInsertion) this this.GetMathNextInsertion this.SetMathNextInsertion
        member this.GetMathPainter world : MathPainter = this.Get (nameof this.MathPainter) world
        member this.MathPainter = lensReadOnly (nameof this.MathPainter) this this.GetMathPainter

type MathFacet () =
    inherit Facet (false, false, false)

    static let rec findNextPlaceholder (mathList: MathList) =
        (None, mathList) ||> Seq.foldi (fun i found atom ->
            match found with
            | Some _ -> found
            | None ->
                match atom with
                | :? Atoms.Placeholder ->
                    Some (MathListIndex.Level0Index i)
                | :? Atoms.Radical as r ->
                    findNextPlaceholder r.Degree |> Option.map (fun subIndex -> MathListIndex.IndexAtLocation (i, MathListSubIndexType.Degree, subIndex))
                    |> Option.orElseWith (fun () -> findNextPlaceholder r.Radicand |> Option.map (fun subIndex -> MathListIndex.IndexAtLocation (i, MathListSubIndexType.Radicand, subIndex)))
                | :? Atoms.Fraction as f ->
                    findNextPlaceholder f.Numerator |> Option.map (fun subIndex -> MathListIndex.IndexAtLocation (i, MathListSubIndexType.Numerator, subIndex))
                    |> Option.orElseWith (fun () -> findNextPlaceholder f.Denominator |> Option.map (fun subIndex -> MathListIndex.IndexAtLocation (i, MathListSubIndexType.Denominator, subIndex)))
                | :? Atoms.Inner as inner ->
                    findNextPlaceholder inner.InnerList |> Option.map (fun subIndex -> MathListIndex.IndexAtLocation (i, MathListSubIndexType.Inner, subIndex))
                | _ -> None
                |> Option.orElseWith (fun () -> findNextPlaceholder atom.Subscript |> Option.map (fun subIndex -> MathListIndex.IndexAtLocation (i, MathListSubIndexType.Subscript, subIndex)))
                |> Option.orElseWith (fun () -> findNextPlaceholder atom.Superscript |> Option.map (fun subIndex -> MathListIndex.IndexAtLocation (i, MathListSubIndexType.Superscript, subIndex)))
        )

    static let updateMath (entity : Entity) world =
        let strokeThickness = entity.GetStrokeThickness world
        entity.SetOverflow strokeThickness world
        let painter = entity.GetMathPainter world
        painter.TextColor <- entity.GetFillColor world
        painter.FontSize <- entity.GetMathFontSize world
        painter.LaTeX <- entity.GetLaTeX world
        entity.SetXtensionPropertyWithoutEvent (nameof Entity.LaTeX) painter.LaTeX world
        entity.SetMathNextInsertion (findNextPlaceholder painter.Content) world
        let size = painter.Measure 0f
        if size.Width > 0f then
            entity.SetSize (v3 size.Width size.Height 0f) world
            let scale = entity.GetScale world
            let width = size.Width * scale.X
            let height = size.Height * scale.Y
            if width > 0f && height > 0f then
                let canvas = NuCanvas (width, height, strokeThickness)
                canvas.Scale (1f / width, -1f / height)
                painter.Draw (canvas, width * -0.5f, (painter.Display.Ascent - painter.Display.Descent) * (entity.GetScale world).Y * 0.5f)
                entity.SetTessellations canvas.Tessellations world
        Cascade

    static member Properties =
        [define Entity.OverflowAbsolute true
         nonPersistent Entity.Tessellations (List ())
         define Entity.LaTeX ""
         define Entity.MathFontSize 12f
         define Entity.FillColor Color.White
         define Entity.StrokeColor Color.Zero
         define Entity.StrokeThickness 2f
         nonPersistent Entity.MathPainter Unchecked.defaultof<MathPainter>
         nonPersistent Entity.MathNextInsertion None]

    override this.Register (entity, world) =
        entity.Set (nameof Entity.MathPainter) (MathPainter ()) world
        for propertyName in
            [nameof Entity.Size; nameof Entity.Scale; nameof Entity.LaTeX; nameof Entity.MathFontSize
             nameof Entity.FillColor; nameof Entity.StrokeColor; nameof Entity.StrokeThickness] do
            World.sense (constant $ updateMath entity) (entity.ChangeEvent propertyName) entity (nameof MathFacet) world
        updateMath entity world |> ignore<Handling>

    override this.Render (_, entity, world) =
        let t = entity.GetTransform world
        for (tess, transform) in entity.GetTessellations world do
            let mutable t = t
            let mutable transform = transform
            transform.M31 <- transform.M31 * t.Size.X
            transform.M32 <- transform.M32 * t.Size.Y
            // combine all transformations: stored transform * entity rotation * entity translation
            let combinedTransform =
                transform
                * Matrix3x2.CreateRotation t.Rotation.Angle2d
                * Matrix3x2.CreateTranslation (t.Position.X, t.Position.Y)

            // extract all components from the combined transform
            t.Position <- v3 combinedTransform.M31 combinedTransform.M32 t.Position.Z
            
            let scaleX = sqrt (combinedTransform.M11 * combinedTransform.M11 + combinedTransform.M12 * combinedTransform.M12)
            let scaleY = sqrt (combinedTransform.M21 * combinedTransform.M21 + combinedTransform.M22 * combinedTransform.M22)
            t.Scale <- v3 scaleX scaleY t.Scale.Z
            
            let rotation = atan2 combinedTransform.M12 combinedTransform.M11
            t.Rotation <- Quaternion.CreateFromAxisAngle (Vector3.UnitZ, rotation)
            
            World.renderContour
                { Transform = t
                  ClipOpt = ValueNone
                  Tessellation = tess } world