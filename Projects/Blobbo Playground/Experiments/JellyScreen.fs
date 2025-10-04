namespace BlobboPlayground
open System
open System.Numerics
open nkast.Aether.Physics2D.Collision.Shapes
open nkast.Aether.Physics2D.Dynamics
open Prime
open Nu

type [<Struct>] Particle =
    { Position : Vector2
      Velocity : Vector2 }

type [<Struct>] ParticleState =
    { // assigned during initialize particles
      mutable Position : Vector2 // updated during resolve collisions - parallel for 1 input, parallel for 2 in/output
      mutable Velocity : Vector2 // updated during calculate interaction forces, resolve collisions - parallel for 1 in/output, parallel for 2 in/output
      mutable ScaledParticle : Particle // parallel for 1 input
      // assigned during initialize grid
      mutable Cell : Vector2i // parallel for 1 input
      // assigned during prepare simulation
      mutable Delta : Vector2 // updated during calculate interaction forces, accumulate deltas - parallel for 1 output, parallel for 2 in/output
      mutable PotentialFixtureCount : int // updated during prepare collisions - parallel for 2 input
      mutable PotentialFixtures : Fixture array // updated during prepare collisions - parallel for 2 input
      // assigned during find neighbors
      mutable NeighborCount : int // parallel for 1 output
      mutable Neighbors : ParticleNeighbor array // parallel for 1 output
      }
and [<Struct>] ParticleNeighbor =
    { // assigned during find neighbors
      mutable ParticleIndex : int // parallel for 1 output
      // assigned during calculate pressures
      mutable Distance : single
      // assigned during calculate interaction forces
      mutable AccumulatedDelta : Vector2 // parallel for 1 output
      }

// this extends the Entity API to expose the user-defined properties.
[<AutoOpen>]
module FluidSystemExtensions =
    type Entity with
        member this.Particles = lens (nameof Entity.Particles) this this.GetParticles this.SetParticles
        member this.GetParticles world : FList<Particle> = this.Get (nameof Entity.Particles) world
        member this.SetParticles (value : FList<Particle>) world = this.Set (nameof Entity.Particles) value world
        member this.ParticleSimulatedCount = lensReadOnly (nameof Entity.ParticleSimulatedCount) this this.GetParticleSimulatedCount
        member this.GetParticleSimulatedCount world : int = this.Get (nameof Entity.ParticleSimulatedCount) world
        /// The maximum number of fluid particles allowed in the simulation at any time.
        member this.ParticleSimulatedCountMax = lens (nameof Entity.ParticleSimulatedCountMax) this this.GetParticleSimulatedCountMax this.SetParticleSimulatedCountMax
        member this.GetParticleSimulatedCountMax world : int = this.Get (nameof Entity.ParticleSimulatedCountMax) world
        member this.SetParticleSimulatedCountMax (value : int) world = this.Set (nameof Entity.ParticleSimulatedCountMax) value world
        /// The maximum number of neighboring particles considered for each particle during force and pressure calculations.
        member this.NeighborCountMax = lens (nameof Entity.NeighborCountMax) this this.GetNeighborCountMax this.SetNeighborCountMax
        member this.GetNeighborCountMax world : int = this.Get (nameof Entity.NeighborCountMax) world
        member this.SetNeighborCountMax (value : int) world = this.Set (nameof Entity.NeighborCountMax) value world
        /// The base radius of each fluid particle, used for collision and interaction calculations.
        member this.ParticleRadius = lens (nameof Entity.ParticleRadius) this this.GetParticleRadius this.SetParticleRadius
        member this.GetParticleRadius world : single = this.Get (nameof Entity.ParticleRadius) world
        member this.SetParticleRadius (value : single) world = this.Set (nameof Entity.ParticleRadius) value world
        /// The ideal interaction radius for particles, as a multiple of Entity.ParticleRadius. Particles within this distance are considered neighbors and interact.
        member this.ParticleInteractionScale = lens (nameof Entity.ParticleInteractionScale) this this.GetParticleInteractionScale this.SetParticleInteractionScale
        member this.GetParticleInteractionScale world : single = this.Get (nameof Entity.ParticleInteractionScale) world
        member this.SetParticleInteractionScale (value : single) world = this.Set (nameof Entity.ParticleInteractionScale) value world
        /// The width and height of each grid cell used for spatial partitioning, as a multiple of Entity.ParticleRadius.
        member this.CellScale = lens (nameof Entity.CellScale) this this.GetCellScale this.SetCellScale
        member this.GetCellScale world : single = this.Get (nameof Entity.CellScale) world
        member this.SetCellScale (value : single) world = this.Set (nameof Entity.CellScale) value world
        /// When set to a color, the simulation will render the spatial grid cells for debugging or visualization.
        member this.DrawCellColor = lens (nameof Entity.DrawCellColor) this this.GetDrawCellColor this.SetDrawCellColor
        member this.GetDrawCellColor world : Color option = this.Get (nameof Entity.DrawCellColor) world
        member this.SetDrawCellColor (value : Color option) world = this.Set (nameof Entity.DrawCellColor) value world
        /// The viscosity coefficient for the fluid. Higher values increase damping and reduce jitter.
        member this.Viscosity = lens (nameof Entity.Viscosity) this this.GetViscosity this.SetViscosity
        member this.GetViscosity world : single = this.Get (nameof Entity.Viscosity) world
        member this.SetViscosity (value : single) world = this.Set (nameof Entity.Viscosity) value world
        member this.TestedFixturesCountMax = lens (nameof Entity.TestedFixturesCountMax) this this.GetTestedFixturesCountMax this.SetTestedFixturesCountMax
        member this.GetTestedFixturesCountMax world : int = this.Get (nameof Entity.TestedFixturesCountMax) world
        member this.SetTestedFixturesCountMax (value : int) world = this.Set (nameof Entity.TestedFixturesCountMax) value world
        member this.ParticleDrawnSize = lens (nameof Entity.ParticleDrawnSize) this this.GetParticleDrawnSize this.SetParticleDrawnSize
        member this.GetParticleDrawnSize world : Vector2 = this.Get (nameof Entity.ParticleDrawnSize) world
        member this.SetParticleDrawnSize (value : Vector2) world = this.Set (nameof Entity.ParticleDrawnSize) value world

type FluidSystemDispatcher () =
    inherit Entity2dDispatcher (true, false, false)
    static let positionToCell cellSize (position : Vector2) =
        v2i (floor (position.X / cellSize) |> int) (floor (position.Y / cellSize) |> int)
    static let cellToBox cellSize (cell : Vector2i) = box2 (cell.V2 * cellSize) (v2Dup cellSize)

    // each particle is associated with a cell in a spatial grid for neighbor searching
    static let neighborhood = [|for x in -1 .. 1 do for y in -1 .. 1 do v2i x y|]

    // here we define default property values
    static member Properties =
        [define Entity.Particles FList.empty
         define Entity.ParticleSimulatedCount 0
         define Entity.ParticleSimulatedCountMax 20000
         define Entity.DrawCellColor None
         define Entity.NeighborCountMax 75
         define Entity.ParticleRadius (0.9f * Constants.Engine.Meter2d)
         define Entity.CellScale (0.6f / 0.9f)
         define Entity.ParticleInteractionScale (50f / 0.9f)
         define Entity.Viscosity 0.004f
         define Entity.TestedFixturesCountMax 20
         define Entity.ParticleDrawnSize (v2 2f 2f)
         // Static sprite properties
         define Entity.InsetOpt None
         define Entity.ClipOpt None
         define Entity.StaticImage Assets.Default.StaticSprite
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone]

    // here we define the entity's top-level behavior
    override _.Update (fluidSystem, world) =
        let sourceParticles = fluidSystem.GetParticles world
        if FList.isEmpty sourceParticles then () else
        let maxParticles = fluidSystem.GetParticleSimulatedCountMax world
        let maxNeighbors = fluidSystem.GetNeighborCountMax world
        let particleRadius = fluidSystem.GetParticleRadius world
        let interactionScale = fluidSystem.GetParticleInteractionScale world
        let particleInteractionRadius = particleRadius * interactionScale
        let particleInteractionRadiusSquared = particleInteractionRadius * particleInteractionRadius
        let cellSize = particleRadius * fluidSystem.GetCellScale world
        let viscosity = fluidSystem.GetViscosity world
        let maxFixtures = fluidSystem.GetTestedFixturesCountMax world

        let deltaTime = world.ClockDelta * Constants.Engine.Meter2d
        let gravity = (World.getGravity2d world).V2 / Constants.Engine.Meter2d / 3000f

        let particles = Buffers.ArrayPool<ParticleState>.Shared.Rent maxParticles
        let mutable activeParticles = 0
        let grid = Collections.Generic.Dictionary ()
        for particle in sourceParticles |> Seq.truncate maxParticles do
            // initialize particles
            particles[activeParticles].Position <- particle.Position
            particles[activeParticles].Velocity <- particle.Velocity
            particles[activeParticles].ScaledParticle <-
                { Position = particle.Position * interactionScale
                  Velocity = particle.Velocity * interactionScale }

            // initialize grid
            let cell = positionToCell cellSize particle.Position
            particles[activeParticles].Cell <- cell
            match grid.TryGetValue cell with
            | (true, list) -> grid[cell] <- activeParticles :: list
            | (false, _) -> grid[cell] <- [activeParticles]
            activeParticles <- inc activeParticles

        // parallel for 1
        Threading.Tasks.Parallel.For(0, activeParticles, fun i ->
            // prepare simulation
            let particle = &particles[i]
            particle.Delta <- v2Zero
            particle.PotentialFixtureCount <- 0
            particle.PotentialFixtures <- Buffers.ArrayPool.Shared.Rent maxFixtures

            // find neighbors
            particle.NeighborCount <- 0
            particle.Neighbors <- Buffers.ArrayPool.Shared.Rent maxNeighbors
            let cell = particle.Cell
            for neighbor in
                neighborhood
                |> Seq.collect (fun neighbour -> match grid.TryGetValue (cell + neighbour) with (true, list) -> list | _ -> [])
                |> Seq.truncate maxNeighbors do
                if neighbor <> i then
                    particle.Neighbors[particle.NeighborCount].ParticleIndex <- neighbor
                    particle.NeighborCount <- inc particle.NeighborCount

            // calculate pressures
            let mutable p = 0f
            let mutable pnear = 0f
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors[n]
                let relativePosition = particles[neighbor.ParticleIndex].ScaledParticle.Position - particle.ScaledParticle.Position
                let distanceSquared = relativePosition.MagnitudeSquared
                if distanceSquared < particleInteractionRadiusSquared then
                    neighbor.Distance <- sqrt distanceSquared
                    let oneMinusQ = 1f - (neighbor.Distance / particleInteractionRadius)
                    p <- p + oneMinusQ * oneMinusQ
                    pnear <- pnear + oneMinusQ * oneMinusQ * oneMinusQ
                else neighbor.Distance <- nanf
            let pressure = (p - 5f) / 2f // normal pressure term
            let presnear = pnear / 2f // near particles term

            // calculate interaction forces
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors[n]
                if not (Single.IsNaN neighbor.Distance) then
                    let q = neighbor.Distance / particleInteractionRadius
                    let oneMinusQ = 1f - q
                    let factor = oneMinusQ * (pressure + presnear * oneMinusQ) / (2f * neighbor.Distance)
                    let relativePosition = particles[neighbor.ParticleIndex].ScaledParticle.Position - particle.ScaledParticle.Position
                    let d = relativePosition * factor
                    let relativeVelocity = particles[neighbor.ParticleIndex].ScaledParticle.Velocity - particle.ScaledParticle.Velocity
                    let factor = viscosity * oneMinusQ * deltaTime
                    let d = d - relativeVelocity * factor
                    neighbor.AccumulatedDelta <- d
                    particle.Delta <- particle.Delta - d
                else neighbor.AccumulatedDelta <- v2Zero
            particle.Velocity <- particle.Velocity + gravity
        ) |> fun result -> assert result.IsCompleted

        // accumulate deltas
        for i in 0 .. dec activeParticles do
            let particle = &particles[i]
            for n in 0 .. dec particle.NeighborCount do
                let neighbor = &particle.Neighbors[n]
                particles[neighbor.ParticleIndex].Delta <- particles[neighbor.ParticleIndex].Delta + neighbor.AccumulatedDelta
        for i in 0 .. dec activeParticles do
            particles[i].Delta <- particles[i].Delta / interactionScale * Constants.Engine.Meter2d // JELLY: ( * Constants.Engine.Meter2d) makes jelly
            
        // prepare collisions
        World.queryBodies2d (fluidSystem.GetBounds world) (fun fixture ->
            let toPixel x = x * Constants.Engine.Meter2d
            let mutable aabb = Unchecked.defaultof<_>
            let mutable transform = Unchecked.defaultof<_>
            fixture.Body.GetTransform &transform
            fixture.Shape.ComputeAABB (&aabb, &transform, 0) // TODO: ChainShape reads childIndex!
            let box =
                    box2 (v2 (toPixel aabb.LowerBound.X) (toPixel aabb.LowerBound.Y))
                         (v2 (toPixel aabb.Width) (toPixel aabb.Height))
            let bottomLeft = positionToCell cellSize box.BottomLeft
            let topRight = positionToCell cellSize box.TopRight
            for gridX in dec bottomLeft.X .. inc topRight.X do // expand grid by one in case some fixtures perfectly align on cell boundary
                for gridY in dec bottomLeft.Y .. inc topRight.Y do
                    match grid.TryGetValue (v2i gridX gridY) with
                    | (true, particleIndexes) ->
                        for i in particleIndexes do
                            let particle = &particles[i]
                            if particle.PotentialFixtureCount < maxFixtures then
                                particle.PotentialFixtures[particle.PotentialFixtureCount] <- fixture
                                particle.PotentialFixtureCount <- inc particle.PotentialFixtureCount
                    | (false, _) -> ()
            true) world

        // parallel for 2 - resolve collisions
        Threading.Tasks.Parallel.For(0, activeParticles, fun i ->
            let toPhysicsV2 (v : Vector2) =
                nkast.Aether.Physics2D.Common.Vector2 (v.X / Constants.Engine.Meter2d, v.Y / Constants.Engine.Meter2d)
            let toPixel x = x * Constants.Engine.Meter2d
            let toPixelV2 (v : nkast.Aether.Physics2D.Common.Vector2) =
                Vector2 (v.X * Constants.Engine.Meter2d, v.Y * Constants.Engine.Meter2d)
            let toPixelV2Unscaled (v : nkast.Aether.Physics2D.Common.Vector2) = Vector2 (v.X, v.Y)
            let particle = &particles[i]
            for f in 0 .. dec particle.PotentialFixtureCount do
                let fixture = particle.PotentialFixtures[f]
                let mutable newPosition = particle.Position + particle.Velocity + particle.Delta |> toPhysicsV2
                if fixture.TestPoint &newPosition then
                    let mutable closestPoint = v2Zero
                    let mutable normal = v2Zero
                    match fixture.Shape with
                    | :? PolygonShape as shape ->
                        let mutable collisionXF = Unchecked.defaultof<_>
                        fixture.Body.GetTransform &collisionXF
                        let mutable shortestDistance = 9999999f // Find closest edge
                        for v in 0 .. dec shape.Vertices.Count do
                            // Transform the shape's vertices from local space to world space
                            let collisionVertex = nkast.Aether.Physics2D.Common.Transform.Multiply (shape.Vertices[v], &collisionXF) |> toPixelV2
                            // Transform the shape's normals using the rotation (Complex) part of the transform
                            let collisionNormal = nkast.Aether.Physics2D.Common.Complex.Multiply (shape.Normals[v], &collisionXF.q) |> toPixelV2Unscaled
                            // Project the vertex position relative to the particle position onto the edge's normal to find the distance
                            let distance = Vector2.Dot (collisionNormal, collisionVertex - particle.Position)
                            if distance < shortestDistance then
                                shortestDistance <- distance
                                // Push the particle out of the shape in the direction of the closest edge's normal
                                closestPoint <- collisionNormal * distance + particle.Position
                                normal <- collisionNormal
                        particle.Position <- closestPoint + 0.05f * normal
                    | :? CircleShape as shape ->
                        // Push the particle out of the circle by normalizing the circle's center relative to the particle position,
                        // and pushing the particle out in the direction of the normal
                        let center = shape.Position + fixture.Body.Position |> toPixelV2
                        normal <- (particle.Position - center).Normalized
                        closestPoint <- center + normal * (toPixel shape.Radius / normal.Magnitude);
                        particle.Position <- closestPoint + 0.05f * normal
                    | _ -> () // TODO : EdgeShape, ChainShape

                    particle.Velocity <- (particle.Velocity - 1.2f * Vector2.Dot (particle.Velocity, normal) * normal) * 0.85f
                    particle.Delta <- v2Zero
        ) |> fun result -> assert result.IsCompleted
        
        // move particles
        let bounds = (fluidSystem.GetBounds world).Box2
        let mutable newParticles = []
        for i in 0 .. dec activeParticles do
            let particle = &particles[i]
            let newVelocity = particle.Velocity + particle.Delta
            let newPosition = particle.Position + newVelocity + particle.Delta
            if bounds.Contains newPosition <> ContainmentType.Disjoint then
                newParticles <- { Position = newPosition; Velocity = newVelocity } :: newParticles
            else
                activeParticles <- dec activeParticles
            
            Buffers.ArrayPool.Shared.Return particle.PotentialFixtures
            Buffers.ArrayPool.Shared.Return particle.Neighbors
        Buffers.ArrayPool.Shared.Return particles
        fluidSystem.SetParticles (FList.ofList newParticles) world
        fluidSystem.Set (nameof Entity.ParticleSimulatedCount) activeParticles world
    
    override _.Render (_, fluidSystem, world) =
        let cellSize = fluidSystem.GetParticleRadius world * fluidSystem.GetCellScale world
        let drawCells = fluidSystem.GetDrawCellColor world
        let grid = Collections.Generic.HashSet ()

        let staticImage = fluidSystem.GetStaticImage world
        let insetOpt = match fluidSystem.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
        let clipOpt = fluidSystem.GetClipOpt world |> Option.toValueOption
        let color = fluidSystem.GetColor world
        let blend = fluidSystem.GetBlend world
        let emission = fluidSystem.GetEmission world
        let flip = fluidSystem.GetFlip world
        let drawnSize = fluidSystem.GetParticleDrawnSize world
        let mutable transform = Transform.makeIntuitive false v3Zero v3One v3Zero drawnSize.V3 v3Zero (fluidSystem.GetElevation world)
        for p in fluidSystem.GetParticles world do
            transform.Position <- p.Position.V3
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
            if drawCells.IsSome then grid.Add (positionToCell cellSize p.Position) |> ignore

        match drawCells with
        | Some color ->
            transform.Elevation <- transform.Elevation - 1f
            transform.Size <- v3Dup cellSize
            let staticImage = Assets.Default.White
            for g in grid do
                let box = cellToBox cellSize g
                transform.Position <- box.Center.V3
                World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
        | None -> ()

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type JellyScreenDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    // here we define the screen's top-level behavior
    override _.Process (_, liquidSim, world) =
        World.beginGroup "Liquid Sim" [] world
        // Create test geometry
        let scale = 20f
        World.doBlock2d "Bottom"
            [Entity.Size .= v3 12f 1f 0f * scale * 2f 
             Entity.Position .= v3 0f -7f 0f * scale
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Color.DeepSkyBlue] world |> ignore
        World.doBlock2d "Left"
            [Entity.Size .= v3 1f 10f 0f * scale * 2f
             Entity.Position .= v3 -11f 0f 0f * scale
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Color.DeepSkyBlue] world |> ignore
        World.doBlock2d "Right"
            [Entity.Size .= v3 1f 10f 0f * scale * 2f
             Entity.Position .= v3 11f 0f 0f * scale
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Color.DeepSkyBlue] world |> ignore
        World.doBlock2d "Ramp"
            [Entity.Size .= v3 5.5f 0.5f 0f * scale * 2f
             Entity.Position .= v3 6f 2f 0f * scale
             Entity.Rotation .= Quaternion.CreateFromAngle2d 0.25f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Color.DeepSkyBlue] world |> ignore
        World.doSphere2d "Circle"
            [Entity.Size .= v3 2f 2f 0f * scale * 2f
             Entity.Position .= v3 0f 0.2f 0f * scale
             Entity.Color .= Color.DeepSkyBlue] world |> ignore
        
        World.doEntity<FluidSystemDispatcher> "Fluid System"
            [Entity.Size .= v3 640f 640f 0f
             Entity.StaticImage .= Assets.Default.Ball
             Entity.DrawCellColor .= Some Color.LightBlue] world
        let fluidSystem = world.DeclaredEntity

        World.doText $"Particle Count"
            [Entity.Position .= v3 255f 160f 0f
             Entity.Text @= $"{fluidSystem.GetParticleSimulatedCount world} Particles"
             Entity.Elevation .= 1f] world
        if World.doButton $"Clear"
            [Entity.Position .= v3 255f 130f 0f
             Entity.Text .= "Clear"
             Entity.Elevation .= 1f] world then
            fluidSystem.SetParticles FList.empty world


        if liquidSim.GetSelected world && world.Advancing then
            let mouse = World.getMousePosition2dWorld false world
            if World.isMouseButtonDown MouseLeft world then
                // create particles
                let createParticle particles =
                    let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * Constants.Engine.Meter2d
                    FList.cons { Position = mouse + jitter; Velocity = v2Zero } particles
                fluidSystem.Particles.Map (createParticle >> createParticle >> createParticle >> createParticle) world
            if World.isMouseButtonDown MouseRight world then
                // delete particles
                fluidSystem.Particles.Map (
                    FList.filter (
                        _.Position
                        >> (box2 (mouse - v2Dup (Constants.Engine.Meter2d / 2f)) (v2Dup Constants.Engine.Meter2d)).Contains
                        >> (=) ContainmentType.Disjoint)) world
            if World.isMouseButtonDown MouseMiddle world then
                // summon a rigid body
                World.doSphere2d "Mouse Sphere"
                    [Entity.Position @= mouse.V3
                     Entity.Size .= v3 64f 64f 0f] world |> ignore

        World.endGroup world
        // process camera as last task
        World.setEye2dCenter (v2 60f 0f) world