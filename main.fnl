(global state {})
(local alive [1 1 1])
(local dead [0 0 0])

(fn make-state []
  ;; initialize global state variable
  (let [(width height) (love.graphics.getDimensions)]
    (set state ;; width and height are inclusive
         {:width (- width 1)
          :height (- height 1)
          :side 0
          :board {0 (love.image.newImageData width height)
                  1 (love.image.newImageData width height)}})
    (let [board (. state.board state.side)
          {: width : height} state]
      (for [_ 1 (* width height)] ; populate random pixels on side 0
        (let [x (love.math.random width)
              y (love.math.random height)]
          (board:setPixel x y (unpack alive)))))))

(fn sum-neighbors [{: x : y : board : width : height}]
  ;; returns a sum of how many neighbors are alive
  (let [neighbors [[-1 -1] [-1 0] [-1 1] [0 1] [0 -1] [1 1] [1 -1] [1 0]]]
    (var sum 0)
    (let [width (+ width 1) ; exclusive
          height (+ height 1)] ; exclusive
      (accumulate [sum 0 _ [i j] (ipairs neighbors)]
        (let [col (% (+ x i) width)
              row (% (+ y j) height)]
          (+ sum (board:getPixel col row)))))))

(fn get-next-color [...]
  ;; returns the new color for a pixel in [r g b] form
  (let [{: x : y : board} ...]
    (case [(sum-neighbors ...) (pick-values 3 (board:getPixel x y))] ; multi value getPixel must be last!
      ;; rule 1: live cell with less than 2 live neighbors dies
      (where [neighbors 1 1 1] (< neighbors 2))
      dead
      ;; rule 2: live cell with 2 or 3 live neighbors lives
      (where [neighbors 1 1 1] (or (= neighbors 2) (= neighbors 3)))
      alive
      ;; rule 3: live cell with more than 3 live neighbors dies
      (where [neighbors 1 1 1] (> neighbors 3))
      dead
      ;; rule 4: dead cell with 3 live neighbors lives
      (where [neighbors 0 0 0] (= neighbors 3))
      alive
      ;; no change
      [_ r g b]
      [r g b])))

(fn love.load []
  ;; start a thread listening on stdin
  (: (love.thread.newThread "require('love.event')
     while true do love.event.push('stdin', io.read('*line')) end") :start)
  (love.window.setTitle "Game of Life")
  (let [width 400
        height 400]
    (love.window.setMode width height))
  (make-state) ; start drawing
  )

(fn love.handlers.stdin [line]
  ;; evaluate lines read from stdin as fennel code
  (let [(ok val) (pcall fennel.eval line)]
    (print (if ok (fennel.view val) val))))

(fn love.draw []
  ;; draws the current board and generates the next board
  (let [board (. state.board state.side)
        image (love.graphics.newImage board)
        next-side (% (+ state.side 1) 2)
        next-board (. state.board next-side)]
    (love.graphics.draw image 0 0)
    (set state.side next-side)
    (let [{: width : height} state]
      (for [x 0 width]
        (for [y 0 height]
          (next-board:setPixel x y
                               (unpack (get-next-color {: x
                                                        : y
                                                        : board
                                                        : width
                                                        : height}))))))))
