namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open Nu.Particles
open BlobboPlayground

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type BlobboPlaygroundPlugin () =
    inherit NuPlugin ()

    override this.MakeEmitters =
        let squareTrailMaker (time : GameTime) (lifeTimeOpt : GameTime) (particleLifeTimeMaxOpt : GameTime) (particleRate : single) (particleMax : int) =
            let image = Assets.Default.White
            let particleSeed =
                { Life = Life.make GameTime.zero particleLifeTimeMaxOpt
                  Body = Body.defaultBody
                  Offset = v3Zero
                  Size = v3One
                  Inset = box2Zero
                  Color = Color.One
                  Emission = Color.Zero
                  Flip = Unflipped }
            let particleInitializer = fun _ (emitter : BasicStaticSpriteEmitter) ->
                let particle = emitter.ParticleSeed
                particle.Body.Position <- emitter.Body.Position
                particle
            let particleBehavior = fun time (emitter : BasicStaticSpriteEmitter) ->
                let watermark = emitter.ParticleWatermark
                let mutable index = 0
                while index <= watermark do
                    let particle = &emitter.ParticleRing.[index]
                    let progress = single (Life.getProgress time particle.Life)
                    let initialSize = emitter.ParticleSeed.Size
                    particle.Size <- initialSize * (1.0f - progress)
                    particle.Color.A <- single (1.0f - progress)
                    index <- inc index
                Output.empty
            BasicStaticSpriteEmitter.make
                time Body.defaultBody false 0.0f Transparent None image
                lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax particleSeed
                Constraint.empty particleInitializer particleBehavior Behaviors.empty
                (fun _ _ -> Output.empty) Behaviors.empty
            :> Emitter
        base.MakeEmitters |> Map.add "SquareTrail" squareTrailMaker

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetGameState Splash world)
             ("Title", fun world -> Game.SetGameState Title world)
             ("Scene01_BlobboThrow", fun world ->
                Simulants.Scene01_BlobboThrow.SetGameplayState Playing world
                Game.SetGameState Scene01_BlobboThrow world)
             ("Scene02_BoxRewind", fun world ->
                Simulants.Scene02_BoxRewind.SetGameplayState Playing world
                Game.SetGameState Scene02_BoxRewind world)
             ("Scene06_M1ControlStudy", fun world ->
                Simulants.Scene06_M1ControlStudy.SetGameplayState Playing world
                Game.SetGameState Scene06_M1ControlStudy world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]
    override this.MakePhysicsEngine2d () = Box2dNetPhysicsEngine.make (Constants.Physics.GravityDefault * Constants.Engine.Meter2d)
    override this.MakePhysicsEngine2dRenderContext segments circles eyeBounds =
        { new Box2dNetPhysicsEngineRenderContext with
            override this.DrawLine (start, stop, color) =
                match segments.TryGetValue color with
                | (true, segmentList) -> segmentList.Add (start, stop)
                | (false, _) -> segments.Add (color, Collections.Generic.List [struct (start, stop)])
            override this.DrawCircle (center, radius, color) =
                match circles.TryGetValue struct (color, radius) with
                | (true, circleList) -> circleList.Add center
                | (false, _) -> circles.Add (struct (color, radius), Collections.Generic.List [center])
            override _.EyeBounds = eyeBounds }