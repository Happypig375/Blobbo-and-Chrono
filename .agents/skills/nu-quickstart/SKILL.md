---
name: nu-quickstart
description: >-
  Start or extend Nu facets, ImSim screen dispatchers, event wiring, GUI controls, and body joints.
  Use for an unfamiliar Nu entity pattern; do not use as generic F# guidance or as a substitute for
  checking the current Nu source.
---

# Nu quickstart

Read [`Standard.md`](../../../Standard.md) and the repository-root `AGENTS.md`. These snippets are
orientation aids, not an API snapshot. Before editing, find the closest current analogue in the checked-out
Nu source and one game or sample call site. Match its dispatcher, property, event-address, lifecycle, and
project-file conventions.

## Choose the owning Nu layer

- **Dispatcher:** behavior intrinsic to an entity, group, screen, or game type.
- **Facet:** reusable entity capability with properties, events, and registration lifetime.
- **ImSim declaration:** a screen-local entity tree reconstructed through Nu's declaration lifecycle.
- **Typed domain function:** deterministic logic that does not need World, entities, or mutable runtime state.
- **Service or adapter:** external I/O, browser, capture, native, or provider orchestration outside dispatchers.

Do not create a facet, property, event, or service merely to forward another member. Inspect existing defaults
and omit declarations that repeat them.

## Facet orientation

The extension module normally precedes the facet type. Verify the surrounding file's exact helpers and event
address shape.

```fsharp
module [<AutoOpen>] MyFacetExtensions =
    type Entity with
        member this.GetX world : T = this.Get (nameof this.X) world
        member this.SetX (value : T) world = this.Set (nameof this.X) value world
        member this.X = lens (nameof this.X) this this.GetX this.SetX
        member this.MyEvent = stoa<Payload> "My/Event" --> this

/// Provides the non-obvious capability and contract.
type MyFacet () =
    inherit Facet (false, false, false)
    static let handleMyEvent (_ : Event<Payload, Entity>) world = Cascade
    static member Properties = [define Entity.X defaultValue]
    override this.Register (entity, world) =
        World.sense handleMyEvent entity.MyEvent entity (nameof MyFacet) world
```

Checklist:

- use a distinctive public property/event name;
- register at the lifetime that owns cleanup;
- return `Cascade` unless intentionally resolving propagation;
- keep the handler static when it does not require facet instance state;
- test the behavior through an entity that actually carries the facet.

## ImSim screen dispatcher orientation

```fsharp
type MyScreenDispatcher () =
    inherit ScreenDispatcherImSim ()
    static member Properties = [define Screen.State defaultValue]
    override this.Process (selectionResults, screen, world) =
        if screen.GetSelected world then
            World.beginGroup "Group" [] world
            // World.do* / World.begin* / World.end* declarations.
            World.endGroup world
```

At the current Nu revision, ImSim property operators have different declaration lifetimes:

- `.=` supplies a static declared value and is reapplied on code reload;
- `|=` initializes once rather than reapplying on code reload;
- `@=` evaluates a dynamic value during declaration processing.

Verify these semantics in current operator definitions before relying on edge behavior. For an imperative
init-only effect, first look for the existing declaration lifecycle signal such as
`world.DeclaredInitializing`; do not add a persistent flag that duplicates it. Every `begin*` must have the
matching `end*` on every executed path.

## Event wiring orientation

```fsharp
// Define:    this.Evt = stoa<T> "Name/Event" --> this
// Facet:     World.sense fn entity.Evt entity (nameof MyFacet) world
// Entity:    World.monitor fn entity.Evt entity world
// Manual:    World.subscribe fn eventAddress entity world
// Publish:   World.publish payload entity.Evt entity world
// Handler:   Event<T, Entity> -> World -> Signal
```

Choose subscription lifetime deliberately. Use manual subscription only when the owner also has an explicit
cleanup path. Preserve established event naming; do not bundle drive-by event renames with unrelated work.

## ImSim body-joint orientation

Backend constructors and record fields are volatile. Copy the shape from a current joint in the checked-out
source rather than pasting this section into code.

```fsharp
World.doBodyJoint2d "Joint"
    [Entity.BodyJointTarget .= stoa "^/BodyA"
     Entity.BodyJointTarget2 .= stoa "^/BodyB"
     Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint =
        fun toPhysics _ bodyIdA bodyIdB world ->
            let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
            jointDef.``base``.bodyIdA <- bodyIdA
            jointDef.``base``.bodyIdB <- bodyIdB
            jointDef.length <- toPhysics restLength
            B2Joints.b2CreateDistanceJoint (world, &jointDef) }] world
```

Confirm the current conversion functions, native argument passing, target addresses, joint lifetime, and
world ownership. Keep physical constants shared and named. Preserve geometry and mechanics before tuning
damping or adding corrective impulses.

## ImSim GUI orientation

```fsharp
if World.doButton "Action" [Entity.Text .= "Act"] world then ()
World.doText "Status" [Entity.Text @= statusText] world
World.beginPanel "Panel" [Entity.Layout .= Flow (FlowDownward, FlowUnlimited)] world
// Nested controls.
World.endPanel world
World.doFillBar "Progress" [Entity.Fill @= progress] world
```

Use stable declaration names, bounded collections, and typed state outside the view declaration. A control is
not validation by itself: run the project, exercise it, inspect runtime logs, and test code reload in Gaia
when the pattern participates in editor reload.