---
name: nu-quickstart
description: >-
  Start or extend Nu facets, ImSim screen dispatchers, event wiring, GUI controls, and Box2D joints.
  Use when implementing an unfamiliar Nu entity pattern, not for generic F# code.
---

# Nu quickstart

Follow [`Standard.md`](../../../Standard.md) and adapt these patterns to the surrounding code rather
than treating the examples as isolated boilerplate.

## Facet

The type remains outside the auto-open module.

```fsharp
module [<AutoOpen>] MyFacetExtensions =
    type Entity with
        member this.GetX world : T = this.Get (nameof this.X) world
        member this.SetX (value : T) world = this.Set (nameof this.X) value world
        member this.X = lens (nameof this.X) this this.GetX this.SetX
        member this.MyEvent = stoa<Payload> "Name/Event" --> this

/// Brief doc comment.
type MyFacet () =
    inherit Facet (false, false, false)
    static let handleEvent (_ : Event<_, Entity>) world = Cascade
    static member Properties = [define Entity.X defaultValue]
    override this.Register (entity, world) =
        World.sense handleEvent entity.MyEvent entity (nameof MyFacet) world
```

## ImSim screen dispatcher

```fsharp
type MyScreenDispatcher () =
    inherit ScreenDispatcherImSim ()
    static member Properties = [define Screen.State defaultValue]
    override this.Process (selectionResults, screen, world) =
        if screen.GetSelected world then
            World.beginGroup "Group" [] world
            // World.do* / World.begin* / World.end* declarations here.
            World.endGroup world
```

ImSim property operators have distinct lifecycle behavior:

- `.=` sets a static value and reapplies it on code reload.
- `|=` initializes once and does not reapply on code reload.
- `@=` binds a value dynamically each frame.

For an imperative init-only effect rather than a property declaration, use
`world.DeclaredInitializing`.

## Event wiring

```fsharp
// Define:    this.Evt = stoa<T> "Name/Event" --> this
// Subscribe: World.sense fn entity.Evt entity (nameof Facet) world // facet lifetime
//            World.monitor fn entity.Evt entity world             // entity lifetime
//            World.subscribe fn eventAddress entity world         // manual cleanup
// Publish:   World.publish payload entity.Evt entity world
// Callback:  Event<T, Entity> -> World -> Signal (Cascade | Resolve)
```

## ImSim Box2D joint

```fsharp
World.doBodyJoint2d "Name"
    [Entity.BodyJointTarget .= stoa "^/BodyA"
     Entity.BodyJointTarget2 .= stoa "^/BodyB"
     Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint =
        fun toPhysics _ bodyIdA bodyIdB world ->
            let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
            jointDef.``base``.bodyIdA <- bodyIdA
            jointDef.``base``.bodyIdB <- bodyIdB
            jointDef.length <- toPhysics restLength
            jointDef.enableSpring <- true
            jointDef.hertz <- 4.0f
            jointDef.dampingRatio <- 0.5f
            B2Joints.b2CreateDistanceJoint (world, &jointDef) }] world
```

`toPhysics` converts a scalar from pixels to meters. The second conversion argument maps `Vector3`
to `B2Vec2` and can be ignored when the joint does not need a point.

## ImSim GUI controls

```fsharp
if World.doButton "N" [Entity.Text .= "Click"] world then ()
World.doText "N" [Entity.Text @= "Score: " + string score] world
World.beginPanel "N" [Entity.Layout .= Flow (FlowDownward, FlowUnlimited)] world
// Nested controls.
World.endPanel world
World.doFillBar "N" [Entity.Fill @= value] world
```