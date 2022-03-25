(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)


module LabProg2019.Maze

open External
open Gfx
open Engine
open System
open System.Media   



// size and creating of the engine
let W = 85
let H = 35
let engine = new engine (W,H)

/// Sound effect 
let soundH = System.Media.SystemSounds.Hand 
let soundA = System.Media.SystemSounds.Asterisk


let mutable numOfGhosts = 1

[< NoEquality; NoComparison >]
type state = {
                player : sprite
                ghost1: sprite
                ghost2: sprite
                ghost3: sprite
             }

type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    /// Shortcut for creating a path pixel.
    static member path = pixel.filled Color.Black
    static member player = pixel.create (Config.player_pixel_char, Color.Cyan)
    static member ghost = pixel.create (Config.ghost_pixel_char, Color.Blue)

/// Width of the maze
let mutable w = 41 
/// Heigth of the maze
let mutable h = 33

/// Random number generator 
let rnd = System.Random()  
let A = Array2D.create w h false

/// Maze generation and printing algorithm 
type maze (w,h) =
    member this.kill (x,y) = 
            A.[x,y] <- true 
            match (x,y) with 
            //Top left corner
            |(x,y) when x=1 && y=1 -> 
                if A.[x+2,y] && A.[x,y+2] then ()
                else 
                    match rnd.Next(0,2) with 
                    |0 -> if A.[x+2,y] then this.kill (x,y)
                          else 
                                A.[x+1,y] <- true
                                A.[x+2,y] <- true
                                this.kill(x+2,y) 
                    |_ -> if A.[x,y+2] then this.kill (x,y)
                          else 
                                A.[x,y+1] <- true
                                A.[x,y+2] <- true
                                this.kill(x,y+2)
            //Bottom left corner
            |(x,y) when x=1 && y=(h-2) -> 
                if A.[x+2,y] && A.[x,y-2] then () 
                else 
                    match rnd.Next(0,2) with
                    |0 -> if A.[x+2,y] then this.kill (x,y)
                          else  
                                A.[x+1,y] <- true
                                A.[x+2,y] <- true
                                this.kill (x+2,y)
                    |_ -> if A.[x,y-2] then this.kill (x,y)
                          else 
                                A.[x,y-1] <- true
                                A.[x,y-2] <- true
                                this.kill (x,y-2)
            //Top right corner
            |(x,y) when x=(w-2) && y=1 -> 
                if A.[x-2,y] && A.[x,y+2] then ()
                else 
                    match rnd.Next(0,2) with
                    |0 -> if A.[x-2,y] then this.kill (x,y)
                          else  
                                A.[x-2,y] <- true
                                A.[x-1,y] <- true
                                this.kill (x-2,y)
                    |_ -> if A.[x,y+2] then this.kill (x,y)
                          else 
                                A.[x,y+2] <- true
                                A.[x,y+1] <- true 
                                this.kill (x,y+2)
            //Bottom right corner
            |(x,y) when x=(w-2) && y=(h-2) -> 
                if A.[x-2,y] && A.[x,y-2] then ()
                else 
                    match rnd.Next(0,2) with
                    |0 -> if A.[x-2,y] then this.kill (x,y)
                          else   
                                A.[x-1,y] <- true
                                A.[x-2,y] <- true
                                this.kill (x-2,y)
                    |_ -> if A.[x,y-2] then this.kill (x,y)
                          else 
                              A.[x,y-1] <- true
                              A.[x,y-2] <- true
                              this.kill (x,y-2)
            // Top border
            |(x,y) when x<(w-2) && x>1 && y=1 ->
                    if A.[x+2,y] && A.[x,y+2] && A.[x-2,y] then ()
                    else 
                        match rnd.Next(0,3) with
                        |0 -> if A.[x+2,y] then this.kill (x,y)
                              else 
                                    A.[x+1,y] <- true
                                    A.[x+2,y] <- true
                                    this.kill (x+2,y)
                        |1 -> if A.[x-2,y] then this.kill (x,y)
                              else 
                                    A.[x-1,y] <- true
                                    A.[x-2,y] <- true
                                    this.kill (x-2,y)
                        |_ -> if A.[x,y+2] then this.kill (x,y)
                              else 
                                    A.[x,y+1] <- true
                                    A.[x,y+2] <- true
                                    this.kill (x,y+2)
            // Left border
            |(x,y) when y>1 && y<h-2 && x=1 -> 
                if A.[x+2,y] && A.[x,y+2] && A.[x,y-2] then ()
                else 
                    match rnd.Next(0,3) with
                    |0 -> if A.[x+2,y] then this.kill (x,y)
                          else 
                                A.[x+1,y] <- true
                                A.[x+2,y] <- true
                                this.kill (x+2,y)
                    |1 -> if A.[x,y-2] then this.kill (x,y)
                          else 
                                A.[x,y-1] <- true
                                A.[x,y-2] <- true
                                this.kill (x,y-2)
                    |_ -> if A.[x,y+2] then this.kill (x,y)
                          else 
                                A.[x,y+1] <- true
                                A.[x,y+2] <- true
                                this.kill (x,y+2)
            // Bottom border
            |(x,y) when x<(w-2) && x>1 && y=(h-2) ->
                if A.[x+2,y] && A.[x,y-2] && A.[x-2,y] then ()
                else 
                    match rnd.Next(0,3) with
                    |0 -> if A.[x+2,y] then this.kill (x,y)
                          else 
                              A.[x+1,y] <- true
                              A.[x+2,y] <- true
                              this.kill (x+2,y)
                    |1 -> if A.[x-2,y] then this.kill (x,y)
                          else 
                              A.[x-1,y] <- true
                              A.[x-2,y] <- true
                              this.kill (x-2,y)
                    |_ -> if A.[x,y-2] then this.kill (x,y)
                          else 
                              A.[x,y-1] <- true
                              A.[x,y-2] <- true
                              this.kill (x,y-2)
            // Right border
            |(x,y) when y>1 && y<h-2 && x=(w-2) -> 
                if A.[x-2,y] && A.[x,y+2] && A.[x,y-2] then ()
                else 
                    match rnd.Next(0,3) with
                    |0 -> if A.[x-2,y] then this.kill (x,y)
                          else 
                              A.[x-1,y] <- true
                              A.[x-2,y] <- true
                              this.kill (x-2,y)
                    |1 -> if A.[x,y-2] then this.kill (x,y)
                          else 
                              A.[x,y-1] <- true
                              A.[x,y-2] <- true
                              this.kill (x,y-2)
                    |_ -> if A.[x,y+2] then this.kill (x,y)
                          else
                              A.[x,y+1] <- true
                              A.[x,y+2] <- true
                              this.kill (x,y+2)
            // No limitations
            |_ -> if  A.[x-2,y] && A.[x,y+2] && A.[x,y-2] && A.[x+2,y] then ()
                  else match rnd.Next(0,4) with
                       |0 -> if A.[x-2,y] then this.kill (x,y)
                             else 
                                 A.[x-1,y] <- true
                                 A.[x-2,y] <- true
                                 this.kill (x-2,y)
                       |1 -> if A.[x,y-2] then this.kill (x,y)
                             else 
                                 A.[x,y-1] <- true
                                 A.[x,y-2] <- true
                                 this.kill (x,y-2)
                       |2 -> if A.[x+2,y] then this.kill (x,y)
                             else
                                 A.[x+1,y] <- true
                                 A.[x+2,y] <- true
                                 this.kill (x+2,y)
                       |_ -> if A.[x,y+2] then this.kill (x,y)
                             else
                                 A.[x,y+1] <- true
                                 A.[x,y+2] <- true
                                 this.kill (x,y+2)
    /// Checks array for not modified values and calls maze.kill when cell not visited has been found
    member this.hunt (a: bool [,]) =
            for i in 1 .. 2 .. w-2 do
                for j in 1 .. 2 .. h-2 do
                    if A.[i,j] then this.kill (i,j)
                    else this.hunt A
    /// Takes modified array and creates wall sprite with false and path sprite with true
    member this.print () =
        // Exit generation 
        A.[w-1,h-2] <- true 
        engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.Green), w-1, h-2, 0) |> ignore // exit
        for i in 0 .. w-1 do 
            for j in 0 .. h-1 do 
                if A.[i,j] then 
                    engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.Black), i, j, 0) |> ignore //path
                else engine.create_and_register_sprite (image.rectangle (1,1,CharInfo.wall), i, j, 0) |> ignore //wall

/// Delete last element from given list
let rec delete_last lst = 
    match lst with 
    |[] -> failwith "Errore"
    |x::[] -> []
    |x::xs -> x::(delete_last xs)
/// Checks if given x appears inside a given list
let rec isPresent x lst = 
    match lst with 
    |[] -> false
    |y::xs -> if x=y then true 
                else (isPresent x xs)
// Function that checks whether solver is stuck between walls and visited cells 
// Returns true if stuck 
let stuck (x,y) lst= 
    if (A.[x+1,y]=false || (isPresent (x+1,y) lst)) 
        && (A.[x-1,y]=false || (isPresent (x-1,y) lst)) 
        && (A.[x,y+1]=false || (isPresent (x,y+1) lst)) 
        && (A.[x,y-1]=false || (isPresent (x,y-1) lst)) then true
    else false
/// Picks random allowed direction in which to move next
let rec randomDir (x,y) lst= 
    match rnd.Next(0,4) with 
    |0 -> if A.[x+1,y] && (not(isPresent (x+1,y) lst)) then (x+1,y) else (randomDir (x,y) lst)
    |1 -> if A.[x,y+1] && (not(isPresent (x,y+1) lst)) then (x,y+1) else (randomDir (x,y) lst)
    |2 -> if A.[x-1,y] && (not(isPresent (x-1,y) lst)) then (x-1,y) else (randomDir (x,y) lst)
    |_ -> if A.[x,y-1] && (not(isPresent (x,y-1) lst)) then (x,y-1) else (randomDir (x,y) lst)

let rec print_list lst = 
    match lst with 
    |[] -> ()
    |(x,y)::xs -> engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.Green), x, y, 4) |> ignore   
                  print_list xs

let solve (x,y) =
    // Moves the solver inside the maze and add various movement inside the correct list; 
    // Every move is inserted in visisted list and correct movements inside path list 
    let rec moves (x,y) = 
        let rec aux current_cell visited path = 
            if current_cell = (w-1, h-2) then (print_list path)
            else if (stuck current_cell visited) then (aux (List.last path) (visited@[current_cell]) (delete_last path))
                    else (aux (randomDir(current_cell) visited) (visited@[(current_cell)]) (path@[(current_cell)]))
        //Start from player's current positions 
        in aux (x,y) [] [] 
    moves (x,y)

    

/// Function that uses the backtracking algorithm to find the shortes way to reach the player 
let move_ghost (dx,dy) (px,py) =   
    if(dx,dy) = (px,py) then (dx,dy)
    else
        // Remove the current position from the list to avoid errors
        let rec delete_head (x,y) list =
            match list with
            |[] -> []
            |[(a,b)] -> [(a,b)]
            |(a,b)::xs -> if (x,y) = (a,b) then delete_head (x,y) xs
                          else (a,b)::delete_head (x,y) xs
        let rec moves (x,y) = 
            let rec aux current_cell visited path =
                    if current_cell = (dx,dy) then List.head (delete_head (List.head path) path)
                    else if (stuck current_cell visited) then (aux (List.last path) (visited@[current_cell]) (delete_last path))
                            else (aux (randomDir(current_cell) visited) (visited@[(current_cell)]) (path@[(current_cell)]))
                //Start from player's current positions 
            in aux (x,y) [(w-1,h-2)] [] 
        moves (px,py)



/// Function that generates coin on the maze
let C = Array2D.create w h false
let generate_coins(C: bool [,])=
    for i in 1 .. w-2 do
        for j in 1 ..h-2 do
            // Spawn chance 
            if rnd.Next(0,12) <> 1 then ()
            elif A.[i,j] then C.[i,j] <- true
                              engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.DarkYellow), i, j, 2) |> ignore
              else ()

/// If currently on coin then adds +1 to score and deletes coin from maze
let onCoin(x,y,C: bool [,],points) = if C.[x,y] then engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.Black), x, y, 3) |> ignore
                                                     Score <- Score + 1
                                                     C.[x,y] <- false
                                     else ()


/// Player sprite
let player_sprite = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red, pixel.filled Color.Gray),1,1,10)
/// Prints a big fat YOU LOST on the the screen
let you_lost() = 
    let lost = engine.create_and_register_sprite(image.rectangle(W,H,pixel.filled Color.White, pixel.filled Color.Black), 0, 0, 100)
    lost.draw_text ("YOU LOST\nQuit the game to start over",W/2-14,H/2-7, Color.White, Color.Black)

//// DRILL CONFIG
//Drill
let drilltimer = new System.Diagnostics.Stopwatch()
//Cooldown
let cooldowntime = new System.Diagnostics.Stopwatch()
let mutable drill = false
///Activates drill when ready
let trapano() = 
    if Ready && (drill=false) then drill <- true 
                                   drilltimer.Start()
                                   cooldowntime.Start()
    else ()

// Drill sprites
let rd = sprintf "Drill ready (t)"
let act = sprintf "Drilling"
let chr = sprintf "!!!Drill charging!!!"


let main () = 
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =

        // Player state coordinates 
        let  i,j = (int (st.player.x)), (int (st.player.y))
        if (i,j) = (w-1,h-2) then You_Win.main() 
                                  soundH.Play()  
        else ()

        /// Function that moves ghosts accordingly to the players position 
        let handle_ghosts(i,j) =
            
        //Ghost 1
            let g1i,g1j = (int(st.ghost1.x),int(st.ghost1.y))
            let temp1 = move_ghost (i,j) (g1i, g1j)
            let dg1i,dg1j = (fst(temp1) - g1i , snd(temp1) - g1j)

            //Ghost 2
            let g2i,g2j = (int(st.ghost2.x),int(st.ghost2.y))
            let temp2 = move_ghost (i,j) (g2i, g2j)
            let dg2i,dg2j = (fst(temp2) - g2i , snd(temp2) - g2j)

            // Ghost 3
            let g3i,g3j = (int(st.ghost3.x),int(st.ghost3.y))
            let temp3 = move_ghost (i,j) (g3i, g3j)
            let dg3i,dg3j = (fst(temp3) - g3i , snd(temp3) - g3j)

            // Depending on the difficulty checks if ghost ate the player
            match numOfGhosts with
            |1 -> if ((g1i,g1j) = (i,j)) then you_lost()    
                  else ()
            |2 -> if ((g1i,g1j) = (i,j) || (g2i,g2j) = (i,j)) then you_lost()    
                  else ()
            |_ -> if ((g1i,g1j) = (i,j) || (g2i,g2j) = (i,j) || (g3i,g3j) = (i,j)) then you_lost()    
                  else ()

            // Depeending of difficulty moves the ghosts in the maze
            match numOfGhosts with
            |1 -> st.ghost1.move_by(dg1i,dg1j)
            |2 -> st.ghost1.move_by(dg1i,dg1j)
                  st.ghost2.move_by(dg2i,dg2j)
            |_ -> st.ghost1.move_by(dg1i,dg1j)
                  st.ghost2.move_by(dg2i,dg2j)
                  st.ghost3.move_by(dg3i,dg3j)
            


        // Check for coins
        onCoin(i,j,C,Score)

        //Time check 
        let elapsed = int(drilltimer.ElapsedMilliseconds)
        let elapsedcool = int(cooldowntime.ElapsedMilliseconds) 
        if(elapsedcool >= 20000) then Ready <- true
                                      cooldowntime.Reset()
        if (elapsed >= 5000) then drill <- false
                                  Ready <- false
                                  drilltimer.Reset()

        //Score and drill sprites
        let hd = sprintf "Current score: %d" Score 
        screen.draw_text (hd, 52,21, Color.Black, Color.Green)

        if drill then screen.draw_text (act, 52,25, Color.Black, Color.Yellow)
        else if Ready then screen.draw_text (rd, 52,25, Color.Black, Color.Green)
                else screen.draw_text (chr, 52,25, Color.Black, Color.Red)


        //Movement
        let dx, dy =
            // Checks if game is finished 
                    match key.KeyChar with
                    |'w' when drill && j<>1 -> engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Black, pixel.filled Color.Gray),i,j,1) |> ignore
                                               A.[i,j-1] <- true
                                               0.,-1.
                    |'a' when drill && i<>1 -> engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Black, pixel.filled Color.Gray),i,j,1) |> ignore
                                               A.[i-1,j] <- true
                                               -1.,0.
                    |'s' when drill && j<>h-2 -> engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Black, pixel.filled Color.Gray),i,j,1) |> ignore
                                                 A.[i,j+1] <- true
                                                 0.,1.
                    |'d' when drill && i<>w-2 -> engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Black, pixel.filled Color.Gray),i,j,1) |> ignore
                                                 A.[i+1,j] <- true
                                                 1.,0.
                    (*When in front of the exit*)
                    |'d' when drill && i=w-2 && j=h-2 -> engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Black, pixel.filled Color.Gray),i,j,1) |> ignore
                                                         1.,0.
                    |'w' when A.[i,j-1] ->  0.,-1.
                    |'a' when A.[i-1,j] ->  -1.,0.
                    |'s' when A.[i,j+1] ->  0.,1.
                    |'d' when A.[i+1,j] ->  1.,0.
                    |'p' -> solve (i,j)
                            0.,0.                   // password to get maze solution  
                    |'t' when Ready-> trapano()
                                      0.,0.
                    |' ' -> if System.Console.ReadLine() = "help me" then solve (i,j) // passing the players current coordinates
                                                                          soundA.Play()
                                                                          0.,0.
                            else 0.,0.
                    |_ -> 0.,0.
                

        st.player.move_by (dx, dy)
        handle_ghosts(i,j)
        st, key.KeyChar = 'q'

    // Various sprites inside the game 
    let background_textbox = engine.create_and_register_sprite (image.rectangle (35, 10, pixel.filled Color.White, pixel.filled Color.Cyan),43,1,1)
    let mutable textbox = engine.create_and_register_sprite (image.rectangle (35, 6, pixel.filled Color.White, pixel.filled Color.Cyan),43,1,2)
    textbox.draw_text ("Try to solve the maze... \nif you can. ;)\nYou can use WASD \nto explore the dark path         \nPress 'T' to start drilling      ", 1, 1, Color.Black, Color.Cyan)
    let mutable hint = engine.create_and_register_sprite (image. rectangle (20,1,pixel.filled Color.Cyan, pixel.filled Color.Cyan),51,15,1)
    hint.draw_text ("What's the password?",0,0, Color.Black, Color.Cyan)
    let hint_background = engine.create_and_register_sprite (image. rectangle (20,1,pixel.filled Color.Cyan, pixel.filled Color.Cyan),51,15,0)


    /// Depending on the difficulty it draws the correct number of ghosts in the maze 
    let draw_ghosts() = 
        match numOfGhosts with 
        |1 -> {
                player = player_sprite
                ghost1 = engine.create_and_register_sprite (image.rectangle (1, 1, CharInfo.ghost),w-2,h-2,10)
                ghost2 = engine.create_and_register_sprite (image.rectangle (1,1,pixel.filled Color.Black), 1, 1, 0)          
                ghost3 = engine.create_and_register_sprite (image.rectangle (1,1,pixel.filled Color.Black), 1, 1, 0)
              }
        |2 -> {
                player = player_sprite
                ghost1 = engine.create_and_register_sprite (image.rectangle (1, 1, CharInfo.ghost),w-2,h-2,10)
                ghost2 = engine.create_and_register_sprite (image.rectangle (1, 1, CharInfo.ghost),1,h-2,10)
                ghost3 = engine.create_and_register_sprite (image.rectangle (1,1,pixel.filled Color.Black), 1, 1, 0)
                
              }
        |_ -> {
                player = player_sprite
                ghost1 = engine.create_and_register_sprite (image.rectangle (1, 1, CharInfo.ghost),w-2,h-2,10)
                ghost2 = engine.create_and_register_sprite (image.rectangle (1, 1, CharInfo.ghost),1,h-2,10)
                ghost3 = engine.create_and_register_sprite (image.rectangle (1,1,CharInfo.ghost), w-2, 1, 10)
              }

    



    // Initialization of maze 
    let maze = new maze (w,h)
    maze.kill (1,1)
    maze.hunt A
    maze.print ()
    generate_coins(C)
    // Passing draw_ghosts as parameter to correclty use the right state 
    engine.loop_on_key my_update (draw_ghosts())
