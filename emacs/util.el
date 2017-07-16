;;; util.el --- Utility code for snake. I have multiple copies of
;;; snake which work a little differently but this code can still be
;;; shared.

(defun lag13/are-perpendicular (v1 v2)
  "Returns t if V1 and V2 are perpendicular to one another.
According to linear algebra, two vectors are orthogonal if their
dot product is 0."
  (zerop (+ (* (car v1) (car v2))
	    (* (cdr v1) (cdr v2)))))

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
  (cl-loop for s across str
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
  (cl-loop for i from 0 to (1- (* width height))
        collect (cons (1+ (% i width))
                      (1+ (/ i width)))))

(defun lag13/gen-food-position (width height snake-body)
  "Generates a new position for the food."
  (let ((open-grid-positions (cl-set-difference
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

(defun lag13/snake-game-is-over (width height snake-body)
  "Returns t if the game of snake is over."
  (or (lag13/snake-won width height snake-body)
      (lag13/snake-lost width height snake-body)))

(defun lag13/snake-draw-game (width height snake-body food-pos score is-paused)
  "Draws the game of snake."
  (let ((inhibit-read-only t))
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
    (insert "Score: " (number-to-string score))
    (when (lag13/snake-game-is-over width height snake-body)
      (goto-char (point-max))
      (newline)
      (if (lag13/snake-won width height snake-body)
          (insert "Holy shit you won!!")
        (insert "You lost. Then again one usually \"loses\" at snake")))))
