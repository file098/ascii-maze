module LabProg2019.Options

open System
open Gfx 
open Engine 
open Maze 

let W = 85
let H = 35
let engine = new engine (W ,H)

type state = {
    selector : sprite
}
let box_w = 10
let box_h = 1

// selector 
let selector = engine.create_and_register_sprite (image.rectangle (12,3, pixel.filled Color.Cyan),W/2-8,9,1)
// options boxes
let mutable options = engine.create_and_register_sprite (image.rectangle (box_w,box_h, pixel.filled Color.Black, pixel.filled Color.Black), W/2-7, 5, 2)
options.draw_text ("OPTIONS",0,0, Color.Cyan, Color.Black)
let mutable easy = engine.create_and_register_sprite (image.rectangle (box_w,box_h, pixel.filled Color.Black, pixel.filled Color.Black), W/2-7, 10, 2)      
easy.draw_text ("Easy",0,0, Color.Cyan, Color.Black)
let mutable normal = engine.create_and_register_sprite (image.rectangle (box_w,box_h, pixel.filled Color.Black, pixel.filled Color.Black), W/2-7, 15, 2)
normal.draw_text ("Normal",0,0, Color.Cyan, Color.Black)
let mutable hard = engine.create_and_register_sprite (image.rectangle (box_w,box_h, pixel.filled Color.Black, pixel.filled Color.Black), W/2-7, 20, 2)
hard.draw_text ("Hard",0,0, Color.Cyan, Color.Black)
//background sprite 
let mutable background = engine.create_and_register_sprite (image.rectangle (W,H, pixel.filled Color.Black, pixel.filled Color.Black),0,0,1)
background.draw_text ("Press Q to return to Menu",W/2-11,H-5,Color.White)
     

let main () = 
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // Player state coordinates
        let  i,j = (int (st.selector.x)), (int (st.selector.y))
        let dx, dy =
            match key.KeyChar with
            |'w' when j>10 -> 0.,-5.
            |'s' when j<19 -> 0.,5. 
            |' ' when i=W/2-8 && j=9 -> w <- 11
                                        h <- 11
                                        start <- true
                                        numOfGhosts <- 1
                                        Maze.main() // begin new game with new sizes 
                                        0.,0.
            |' ' when i=W/2-8 && j=14 -> w <- 25
                                         h <- 25
                                         start <- true
                                         numOfGhosts <- 2
                                         Maze.main() 
                                         0.,0.
            |' ' when i=W/2-8 && j=19  -> w <- 41
                                          h <- 33
                                          start <- true
                                          numOfGhosts <- 3
                                          Maze.main()
                                          0.,0.
            |_ ->   soundA.Play()
                    0.,0.
        
        
        st.selector.move_by (dx, dy)
        st, key.KeyChar = 'q' 
   
    let st0 = { 
        selector = selector 
        }

    engine.loop_on_key my_update st0