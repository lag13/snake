;;; snake.el --- Implementation of Snake for Emacs

;; Author: Lucas Groenendaal

;;; Commentary:

;; Another implementation of snake where the key presses are defined
;; as key mappings rather than reading the key presses using
;; read-event.

;; TODO: BUGGGG when the game ends and I switch buffers within the
;; timer interval, the game still does one more tick. That is because
;; of how the game loop is set up. I think I should update the game
;; state and then check if it ends the game. There is a similar
;; problem if you leave the snake buffer too early as it is running.
;; Also, if you leave the snake buffer as it is running it seems that
;; going back to the snake buffer does not start up the game again.

(load-file "util.el")

(defconst *lag13/snake-board-width* 20
  "The width of the snake game.")

(defconst *lag13/snake-board-height* 20
  "The height of the snake game.")

(defvar *lag13/snake-direction nil
  "The direction the snake is traveling in.")

(defvar *lag13/snake-body* nil
  "The entire body of the snake.")

(defvar *lag13/snake-food-pos nil
  "Where the food is.")

(defvar *lag13/snake-score nil
  "What the current score is.")

(defvar *lag13/snake-is-paused* nil
  "Whether the game is paused or not.")

(defvar *lag13/snake-timer* nil
  "References the timer which runs the game loop.")

(defun lag13/snake-move-left ()
  "Moves the snake to the left."
  (interactive)
  (setq *lag13/snake-direction* '(-1 . 0)))

(defun lag13/snake-move-right ()
  "Moves the snake to the right."
  (interactive)
  (setq *lag13/snake-direction* '(1 . 0)))

(defun lag13/snake-move-up ()
  "Moves the snake to the up."
  (interactive)
  (setq *lag13/snake-direction* '(0 . -1)))

(defun lag13/snake-move-down ()
  "Moves the snake to the down."
  (interactive)
  (setq *lag13/snake-direction* '(0 . 1)))

(defun lag13/snake-pause ()
  "Pauses the game."
  (interactive)
  (setq *lag13/snake-is-paused* (not *lag13/snake-is-paused*)))

;; As of Emacs 25.1, all special-mode really does is set
;; buffer-read-only so that the buffer cannot be edited. This feature
;; is useful for games.
(define-derived-mode lag13/snake-mode special-mode "lag13/Snake"
  "A mode for playing Snake."
  (add-hook 'kill-buffer-hook 'lag13/snake-cancel-timer)
  (define-key lag13/snake-mode-map (kbd "<left>") 'lag13/snake-move-left)
  (define-key lag13/snake-mode-map (kbd "<right>") 'lag13/snake-move-right)
  (define-key lag13/snake-mode-map (kbd "<up>") 'lag13/snake-move-up)
  (define-key lag13/snake-mode-map (kbd "<down>") 'lag13/snake-move-down)
  (define-key lag13/snake-mode-map (kbd "p") 'lag13/snake-pause))

(defun lag13/snake-update (snake-buffer)
  "Updates the snake game state and draws it."
  (when (and (not *lag13/snake-is-paused*)
             (eq (current-buffer) snake-buffer))
    (if (lag13/snake-game-is-over *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body*)
        (lag13/snake-cancel-timer)
      (let ((snake-new-head (lag13/add-cons-pair *lag13/snake-direction* (car *lag13/snake-body*))))
        (cond
         ((equal *lag13/snake-food-pos snake-new-head)
          (setq *lag13/snake-score* (1+ *lag13/snake-score*))
          (setq *lag13/snake-body* (cons snake-new-head *lag13/snake-body*))
          (setq *lag13/snake-food-pos (lag13/gen-food-position *lag13/snake-board-width *lag13/snake-board-height* *lag13/snake-body*)))
         (t
          (setq *lag13/snake-body* (cons snake-new-head (butlast *lag13/snake-body*))))))))
  (lag13/snake-draw-game *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body* *lag13/snake-food-pos* *lag13/snake-score* *lag13/snake-is-paused*))

(defun lag13/snake-init ()
  "Initializes the game of snake."
  (setq *lag13/snake-body* '((2 . 2) (2 . 1) (1 . 1) (1 . 2)))
  (setq *lag13/snake-direction* '(0 . 1))
  (setq *lag13/snake-food-pos* (lag13/gen-food-position *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body*))
  (setq *lag13/snake-score* 0)
  (setq *lag13/snake-is-paused* nil)
  (lag13/snake-draw-game *lag13/snake-board-width* *lag13/snake-board-height* *lag13/snake-body* *lag13/snake-food-pos* *lag13/snake-score* *lag13/snake-is-paused*)
  (setq *lag13/snake-timer* (run-with-timer 2 2 'lag13/snake-update (current-buffer))))

(defun lag13/snake-cancel-timer ()
  (cancel-timer *lag13/snake-timer*))

(defun lag13/snake ()
  "Starts the game Snake."
  (interactive)
  (switch-to-buffer "*lag13/snake*")
  (lag13/snake-mode)
  (lag13/snake-init))
