;;; snake-read-event.el --- Implementation of Snake for Emacs

;; Author: Lucas Groenendaal

;;; Commentary:

;; It's worth noting that emacs already has a built in snake game (of
;; course it does), I just felt like making my own. From my
;; experimentation with making games/animations I've learned a couple
;; things:

;; 1. When elisp code is being executed any keys you press are ignored
;; unless you explicitly read them in your code. One exception to this
;; is C-g which will stop any executing lisp code.

;; 2. When elisp code is being executed the buffer does NOT get
;; redrawn unless you explicitly tell it to.

;; After a bit of research, it seems there are two (although there are
;; probably more) approaches you can take when making games. I believe
;; the first one is preferred. This file does (2).

;; 1. Create key mappings to control the game state. If the game can
;; update itself without human input (like with snake) then use the
;; "run-with-timer" function to execute the game loop in the
;; background. One thing I'm a little unsure about is if there is the
;; potential for race conditions here since different key presses
;; could all be changing some global structure.

;; 2. Write code to read input yourself:
;; http://dantorop.info/project/emacs-animation/lisp8.html. The
;; downside with this approach over the other is that since your game
;; is reading all input, you can't execute key bindings like C-x C-o
;; to change windows. So once you start the game then you pretty much
;; must finish it. I'm also not sure how to read input asynchronously
;; so it does not effect the speed of the game.

;; Here are some functions+variables which seem useful for
;; games+animations with emacs:

;; sit-for: sleeps for some amount of time or until there is input
;; available, whichever comes first, and redraws the screen. As I said
;; earlier, emacs does not consume input while executing a function,
;; unless you do it yourself, meaning that if you have a loop with a
;; "sit-for" inside then when you press a character the "sit-for" will
;; not sleep anymore since there is a character in the input buffer.
;; So the loop will finish as fast as your computer can process it.
;; I'm not entirely sure of the utility of that "feature".

;; sleep-for: sleeps for some amount of time. "sit-for" uses it
;; internally.

;; redisplay: redraws the screen. "sit-for" uses this internally.

;; cursor-type: setting this to nil will hide the cursor.

;; read-event: reads a keypress from the keyboard. It's not documented
;; but it also appears to redraw the buffer.

;; buffer-disable-undo: disables undo in the current buffer. Since
;; games can produce a lot of "frames" it means all those frames
;; wouldn't be stored in emacs' memory.

(load-file "util.el")

;; I originally said it in the go implementation but I've added one
;; more note. I think the best structure for a game loop is:
;; 1. Render (the initial game state should be drawn after all)
;; 2. Sleep (gives the player time to see the game and respond to it's current state
;; 3. Get input
;; 4. Update the game state
;; 5. After the game loop exits (presumably due to a win or loss) redraw the game so the "end" state of the game can be seen
(defun lag13/snake-read-event ()
  "Starts the game of snake."
  (interactive)
  (switch-to-buffer "*lag13/snake-read-event*")
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
    (lag13/snake-draw-game width height snake-body food-pos score nil)))
