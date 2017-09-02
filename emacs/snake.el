;;; snake.el --- Implementation of Snake for Emacs

;; Author: Lucas Groenendaal

;;; Commentary:

;; A different implementation of snake where the key presses are
;; defined as key mappings which I believe is the preferred way of
;; making games (I like it more as well).

(load-file "util.el")

(defconst *lag13/snake-board-width* 15
  "The width of the snake game.")

(defconst *lag13/snake-board-height* 15
  "The height of the snake game.")

(defvar *lag13/snake-direction* nil
  "The direction the snake is traveling in.")

(defvar *lag13/snake-direction-queue* nil
  "A queue of directions which have been inputted. The purpose of having this is so that if a player quickly presses something like \"up\" then \"left\", both keystrokes will be recored")

(defvar *lag13/snake-body* nil
  "The entire body of the snake.")

(defvar *lag13/snake-food-pos* nil
  "Where the food is.")

(defvar *lag13/snake-score* nil
  "What the current score is.")

(defvar *lag13/snake-is-paused* nil
  "Whether the game is paused or not.")

(defvar *lag13/snake-timer* nil
  "References the timer which runs the game loop.")

(defun lag13/snake-previous-entered-direction ()
  "Returns the previously entered direction."
  (if *lag13/snake-direction-queue*
      (car *lag13/snake-direction-queue*)
    *lag13/snake-direction*))

(defun lag13/snake-add-direction-to-queue (dir)
  "Adds a newly entered direction to the queue if the direction
is valid."
  (when (lag13/are-perpendicular dir (lag13/snake-previous-entered-direction))
    (push dir *lag13/snake-direction-queue*)))

(defun lag13/snake-move-left ()
  "Moves the snake to the left."
  (interactive)
  (lag13/snake-add-direction-to-queue '(-1 . 0)))

(defun lag13/snake-move-right ()
  "Moves the snake to the right."
  (interactive)
  (lag13/snake-add-direction-to-queue '(1 . 0)))

(defun lag13/snake-move-up ()
  "Moves the snake to the up."
  (interactive)
  (lag13/snake-add-direction-to-queue '(0 . -1)))

(defun lag13/snake-move-down ()
  "Moves the snake to the down."
  (interactive)
  (lag13/snake-add-direction-to-queue '(0 . 1)))

(defun lag13/snake-pause ()
  "Pauses the game."
  (interactive)
  (setq *lag13/snake-is-paused* (not *lag13/snake-is-paused*)))

;; As of Emacs 25.1, all special-mode really does is set
;; buffer-read-only so that the buffer cannot be edited. This feature
;; is useful for games.
(define-derived-mode lag13/snake-mode special-mode "lag13/Snake"
  "A mode for playing Snake."
  (setq cursor-type nil)
  ;; When I was working on this game I made the mistake of assuming
  ;; that by default add-hook would create a hook local to the snake
  ;; buffer. I was very wrong and it lead to a lot of headache. By
  ;; default, add-hook modifies the global hook. So what was happening
  ;; was that EVERY time a buffer was killed the "timer cancel"
  ;; function was called. From first hand experience it turns out that
  ;; emacs kills a LOT of buffers behind the scenes so this function
  ;; was triggering a lot and failing and causing general weirdness
  ;; (probably in part because the timer variable still held a
  ;; reference to the cancelled timer). That last t paramter makes
  ;; this hook addition only apply in the "snake" buffer.
  (add-hook 'kill-buffer-hook 'lag13/snake-cancel-timer nil t)
  (define-key lag13/snake-mode-map (kbd "a") 'lag13/snake-move-left)
  (define-key lag13/snake-mode-map (kbd "d") 'lag13/snake-move-right)
  (define-key lag13/snake-mode-map (kbd "w") 'lag13/snake-move-up)
  (define-key lag13/snake-mode-map (kbd "s") 'lag13/snake-move-down)
  (define-key lag13/snake-mode-map (kbd "p") 'lag13/snake-pause)
  (define-key lag13/snake-mode-map (kbd "n") 'lag13/snake))

(defun lag13/get-direction ()
  "Set's the current direction that the snake is traveling in."
  (if *lag13/snake-direction-queue*
      (let ((new-dir (car (last *lag13/snake-direction-queue*))))
        (setq *lag13/snake-direction-queue* (butlast *lag13/snake-direction-queue*))
        new-dir)
    *lag13/snake-direction*))

(defun lag13/snake-update (snake-buffer)
  "Updates the snake game state and draws it."
  (when (and (not *lag13/snake-is-paused*)
             (eq (current-buffer) snake-buffer))
    (if (lag13/snake-game-is-over *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body*)
	(lag13/snake-cancel-timer)
      (setq *lag13/snake-direction* (lag13/get-direction))
      (let ((snake-new-head (lag13/add-cons-pair *lag13/snake-direction* (car *lag13/snake-body*))))
        (cond
         ((equal *lag13/snake-food-pos* snake-new-head)
          (setq *lag13/snake-score* (1+ *lag13/snake-score*))
          (setq *lag13/snake-body* (cons snake-new-head *lag13/snake-body*))
          (setq *lag13/snake-food-pos* (lag13/gen-food-position *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body*)))
         (t
          (setq *lag13/snake-body* (cons snake-new-head (butlast *lag13/snake-body*))))))))
  (when (eq (current-buffer) snake-buffer)
    (lag13/snake-draw-game *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body* *lag13/snake-food-pos* *lag13/snake-score* *lag13/snake-is-paused*)))

(defun lag13/snake-init ()
  "Initializes the game of snake."
  ;; The game function could be invoked in the middle of another game
  ;; so we need to clean up this timer, otherwise it will generate
  ;; another one (which makes the game go twice as fast haha).
  (lag13/snake-cancel-timer)
  (setq *lag13/snake-body* '((1 . 1) (1 . 0) (0 . 0) (0 . 1)))
  (setq *lag13/snake-direction* '(0 . 1))
  (setq *lag13/snake-direction-queue* nil)
  (setq *lag13/snake-food-pos* (lag13/gen-food-position *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body*))
  (setq *lag13/snake-score* 0)
  (setq *lag13/snake-is-paused* nil)
  (lag13/snake-draw-game *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body* *lag13/snake-food-pos* *lag13/snake-score* *lag13/snake-is-paused*)
  (setq *lag13/snake-timer* (run-at-time 0.1 0.1 'lag13/snake-update (current-buffer))))

(defun lag13/snake-cancel-timer ()
  "Cancels the timer driving the the game."
  (when *lag13/snake-timer*
    (cancel-timer *lag13/snake-timer*)
    ;; Just good hygine to not leave any references we don't need
    ;; anymore.
    (setq *lag13/snake-timer* nil)))

(defun lag13/snake ()
  "Starts the game Snake."
  (interactive)
  (switch-to-buffer "*lag13/snake*")
  (lag13/snake-mode)
  (lag13/snake-init))
