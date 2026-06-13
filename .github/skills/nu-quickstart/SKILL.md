---
name: nu-quickstart
description: Copy-paste templates for Nu game engine patterns not covered by DeepWiki — facet skeleton, screen dispatcher skeleton, event wiring, Box2D joint creation, ImSim property operators. Use when starting a new dispatcher, facet, joint, or entity.
---

[DeepWiki: Nu Game Engine](https://deepwiki.com/bryanedds/Nu) is the authoritative reference for architecture and concepts. This skill provides only copy-paste templates.

## Facet Skeleton
(The type is outside the module and `[<AutoOpen>]` there is valid syntax.)
```fsharp
module [<AutoOpen>] MyFacetExtensions =
    type Entity with
        member this.GetX world : T = this.Get (nameof this.X) world
        member this.SetX (v : T) world = this.Set (nameof this.X) v world
        member this.X = lens (nameof this.X) this this.GetX this.SetX
        member this.MyEvent = stoa<Payload> "Name/Event" --> this

/// Brief doc comment.
type MyFacet () =
    inherit Facet (false, false, false)
    static let handleEvent (evt : Event<_, Entity>) world = Cascade
    static member Properties = [define Entity.X defaultValue]
    override this.Register (entity, world) =
        World.sense handleEvent entity.MyEvent entity (nameof MyFacet) world
```

## ImSim Screen Dispatcher Skeleton
```fsharp
type MyScreenDispatcher () =
    inherit ScreenDispatcherImSim ()
    static member Properties = [define Screen.State defaultValue]
    override this.Process (selectionResults, screen, world) =
        if screen.GetSelected world then
            World.beginGroup "Group" [] world
            // World.do* / World.begin* / World.end* declarations here
            World.endGroup world
```

## Event Wiring Quick-Ref
```fsharp
// Define:   this.Evt = stoa<T> "Name/Event" --> this
// Subscribe: World.sense fn entity.Evt entity (nameof Facet) world  // facet lifetime
//            World.monitor fn entity.Evt entity world                // entity lifetime
//            World.subscribe fn eventAddr entity world                // manual cleanup
// Publish:  World.publish payload entity.Evt entity world
// Callback: Event<T, Entity> -> World -> Signal (Cascade | Resolve)
```

## ImSim Box2D Joint Creation
```fsharp
World.doBodyJoint2d "Name"
    [Entity.BodyJointTarget .= stoa "^/BodyA"
     Entity.BodyJointTarget2 .= stoa "^/BodyB"
     Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint =
        fun toPhysics _ a b world ->
            let mutable def = B2Joints.b2DefaultDistanceJointDef ()
            def.``base``.bodyIdA <- a; def.``base``.bodyIdB <- b
            def.length <- toPhysics restLength
            def.enableSpring <- true; def.hertz <- 4f; def.dampingRatio <- 0.5f
            B2Joints.b2CreateDistanceJoint (world, &def) }] world
```
`toPhysics: single→single` (px→m), second param is `Vector3→B2Vec2` (unused above as `_`).

## ImSim GUI Controls
```fsharp
if World.doButton "N" [Entity.Text .= "Click"] world then ...  // returns bool
World.doText "N" [Entity.Text @= "Score: " + string s] world
World.beginPanel "N" [Entity.Layout .= Flow (FlowDownward, FlowUnlimited)] world
    // nested controls
World.endPanel world
World.doFillBar "N" [Entity.Fill @= v] world
```
