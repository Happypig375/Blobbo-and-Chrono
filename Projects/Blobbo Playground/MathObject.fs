namespace BlobboPlayground
open System
open System.Numerics
open CSharpMath.Editor
open Nu

module [<AutoOpen>] MathObjectExtensions =
    do ()

type MathObjectDispatcher () =
    inherit Entity2dDispatcherImSim (false, false, false)

    static member Facets =
        [typeof<MathFacet>
         typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.BodyType Dynamic]

    override this.Process (entity, world) =
        for penetration in World.doSubscription "Collision" entity.BodyPenetrationEvent world do
            match penetration.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as other when other.Is<MathObjectDispatcher> world ->
                match entity.GetMathNextInsertion world with
                | Some nextInsertion ->
                    let mutable nextInsertion = nextInsertion
                    let content = (entity.GetMathPainter world).Content
                    for insertAtom in (other.GetMathPainter world).Content do
                        content.InsertAndAdvance (&nextInsertion, insertAtom, MathListSubIndexType.None)
                    entity.SetLaTeX (CSharpMath.Atom.LaTeXParser.MathListToLaTeX content |> string) world
                    other.SetVisible false world
                    other.SetBodyEnabled false world
                    other.SetEnabled false world
                    match entity.GetMathNextInsertion world with
                    | None ->
                        let content = (entity.GetMathPainter world).Content
                        let result = CSharpMath.Evaluation.Evaluate content
                        result.Match
                            (function
                            | :? CSharpMath.Evaluation.MathItem.Entity as mathResult ->
                                entity.SetLaTeX (mathResult.Content.Simplify().Latexise ()) world
                            | _ -> entity.SetColor Color.Red world
                            , fun _ -> entity.SetColor Color.Red world)
                    | Some _ -> ()
                | None -> ()
            | _ -> ()