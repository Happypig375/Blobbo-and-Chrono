﻿namespace SandBox2d
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module rec Simulants =

    let SandBoxScreen = Game / "SandBox"
    let RaceCourseScreen = Game / "RaceCourse"

    let [<Literal>] SceneGroup = "Scene"
    let [<Literal>] BorderEntity = "Border"
