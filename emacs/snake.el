;;; snake.el --- Implementation of Snake for Emacs.

(defun lag13/intersperse (i lst)
  "Intersperse a value I into the list LST."
  (if (cdr lst)
      (cons (car lst) (cons i (lag13/intersperse i (cdr lst))))
    lst))

(defun lag13/make-grid (width height background-char)
  "Creates a grid consisting of the same background character."
  (erase-buffer)
  (dotimes (_ height)
    (let ((unspaced-line (make-list width background-char)))
      (insert (concat (lag13/intersperse ?\s unspaced-line))))
    (newline)))

(defun lag13/draw-char (x y width height char)
  "Draws a character on a grid created by `lag13/make-grid'. No
character is drawn if it is off of the grid."
  (when (and (>= x 1) (<= x width)
             (>= y 1) (<= y height))
    (goto-char (+ (1+ (* 2 (1- x))) (* (1- y) (* 2 width))))
    (delete-char 1)
    (insert char)))

(defun lag13/draw-string (x y width height str)
  "Draws a string on a grid using `lag13/draw-char'."
  (loop for s across str
        for i from x
        do (lag13/draw-char i y width height s)))

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

(defun lag13/snake-draw-game (width height snake-body food-pos score is-paused)
  "Draws the game of snake."
  (lag13/make-grid width height ?.)
  (when food-pos
    (lag13/draw-char (car food-pos) (cdr food-pos) width height ?*))
  (lag13/draw-snake-body width snake-body)
  (when is-paused
    (let ((paused-msg "[Paused]"))
      (lag13/draw-string (- (/ width 2)
                            (/ (length paused-msg) 2))
                         (/ height 2)
                         width
                         height
                         paused-msg)))
 (goto-char (point-max))
  (insert "Score: " (number-to-string score)))

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
         (food-pos (lag13/gen-food-position width height snake-body))
         (score 0)
         (is-paused nil))
    (while (not (or
                 (lag13/snake-won width height snake-body)
                 (lag13/snake-lost width height snake-body)))
      (lag13/snake-draw-game width height snake-body food-pos score is-paused)
      (let ((key (read-event nil nil 0.2)))
        (cond
         ((eq key 'up) (setq direction '(0 . -1)))
         ((eq key 'down) (setq direction '(0 . 1)))
         ((eq key 'left) (setq direction '(-1 . 0)))
         ((eq key 'right) (setq direction '(1 . 0)))
         ((eq key ?p) (setq is-paused (not is-paused)))))
      (unless is-paused
        (let ((snake-new-head (lag13/add-cons-pair direction (car snake-body))))
          (cond
           ((equal food-pos snake-new-head)
            (setq score (1+ score))
            (setq snake-body (cons snake-new-head snake-body))
            (setq food-pos (lag13/gen-food-position width height snake-body)))
           (t
            (setq snake-body (cons snake-new-head (butlast snake-body))))))))
    (lag13/snake-draw-game width height snake-body food-pos score nil)
    (goto-char (point-max))
    (newline)
    (when (lag13/snake-won width height snake-body)
      (insert "Holy shit you won!!"))))
