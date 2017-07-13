;;; snake.el --- Implementation of Snake for Emacs.

(defun lag13/make-grid (width height background-char)
  "Creates a grid consisting of the same background character."
  (erase-buffer)
  (dotimes (_ height)
    (insert (make-string width background-char))
    (newline)))

(defun lag13/draw-char (x y width height char)
  "Draws a character on a grid created by `lag13/make-grid'. No
character is drawn if it is off of the grid."
  (when (and (>= x 1) (<= x width)
             (>= y 1) (<= y height))
    (goto-char (+ x (* (1- y) (1+ width))))
    (delete-char 1)
    (insert char)))

(defun lag13/draw-snake-body (width snake-body)
  "Draws the snake's body."
  (lag13/draw-char (caar snake-body) (cdar snake-body) width height ?@)
  (dolist (s (cdr snake-body))
    (lag13/draw-char (car s) (cdr s) width height ?#)))

(defun lag13/add-cons-pair (x y)
  "Add two cons cells together."
  (cons (+ (car x) (car y))
        (+ (cdr x) (cdr y))))

(defun lag13/gen-all-grid-positions (width height)
  "Generates a list of cons cells for each position in the grid."
  (loop for i from 0 to (1- (* width height))
        collect (cons (1+ (% i width))
                      (1+ (/ i width)))))

(defun lag13/gen-food-position (width height snake-body)
  "Generates a new position for the food."
  (let ((open-grid-positions (set-difference
                              (lag13/gen-all-grid-positions width height)
                              snake-body
                              :test 'equal)))
    (nth (random (length open-grid-positions)) open-grid-positions)))

(defun lag13/snake-won (width height snake-body)
  "Returns t if you won snake."
  (= (* width height)
     (length snake-body)))

(defun lag13/snake-lost (width height snake-body)
  "Returns t if you lost snake."
  (let ((snake-headx (caar snake-body)) (snake-heady (cdar snake-body)))
    (or (member (car snake-body) (cdr snake-body))
        (< snake-headx 1)
        (> snake-headx width)
        (< snake-heady 1)
        (> snake-heady height))))

(defun lag13/snake-draw-game (width height snake-body food-pos)
  "Draws the game of snake."
  (lag13/make-grid width height ?.)
  (when food-pos
    (lag13/draw-char (car food-pos) (cdr food-pos) width height ?*))
  (lag13/draw-snake-body width snake-body))

;; I originally said it in the go implementation but I've added one
;; more note. I think the best structure for a game loop is:
;; 1. Render (the initial game state should be drawn after all)
;; 2. Sleep (gives the player time to see the game and respond to it's current state
;; 3. Get input
;; 4. Update the game state
;; 5. After the game loop exits (presumably due to a win or loss) redraw the game so the "end" state of the game can be seen
(defun lag13/snake ()
  "Starts the game of snake."
  (let* ((cursor-type nil)
         (width 20)
         (height 20)
         (snake-body '((2 . 2) (2 . 1) (1 . 1) (1 . 2)))
         (direction '(0 . 1))
         (food-pos (lag13/gen-food-position width height snake-body)))
    (while (not (or
                 (lag13/snake-won width height snake-body)
                 (lag13/snake-lost width height snake-body)))
      (lag13/snake-draw-game width height snake-body food-pos)
      (let ((key (read-event nil nil 2)))
        (setq direction (cond
                         ((eq key 'up) '(0 . -1))
                         ((eq key 'down) '(0 . 1))
                         ((eq key 'left) '(-1 . 0))
                         ((eq key 'right) '(1 . 0))
                         (t direction)))
        (let ((snake-new-head (lag13/add-cons-pair direction (car snake-body))))
          (cond
           ((equal food-pos snake-new-head)
            (setq snake-body (cons snake-new-head snake-body))
            (setq food-pos (lag13/gen-food-position width height snake-body)))
           (t
            (setq snake-body (cons snake-new-head (butlast snake-body))))))))
    (lag13/snake-draw-game width height snake-body food-pos)
    (goto-char (point-max))
    (if (lag13/snake-won width height snake-body)
        (insert "Holy shit you won!!")
      (insert "You lost"))))
