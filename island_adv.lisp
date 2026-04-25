

(defun create_world_env ()
  '((time . 0)
    (player_pos . (17 . 52))
    (map_size . (64 . 64))
    (message . "")
    ))

(defun get_val (key alist)
  (cdr (assoc key alist)))

(defun set_val (key value alist)
  (cons (cons key value)
        (remove key alist :key #'car :test #'eq)))

;; Get tile attributes at the given coordinates. Lisp does not really need enums, so we use symbols
(defun tile_attributes (x y world_env)
    "Get the tile attributes at the given coordinates"
    (let ((map_size (get_val 'map_size world_env)))
    (cond 
        ((is_tile_water x y) 'water)
        ((is_tile_water (- x 1) y) 'cliff) 
        ((is_tile_water (+ x 1) y) 'beach) 
        ((is_tile_water x (+ y 1)) 'beach)     
        ((is_tile_water x (- y 1)) 'beach)          
        ((< x (/ (car map_size) 2)) 'forest)
        ((< y (/ (cdr map_size) 2)) 'plains)
        ('t 'empty)
    )
    )
)

(defun pairwise (lst)
  "Group a list into a list of cons pairs."
  (if lst
      (cons (cons (first lst) (second lst))
            (pairwise (cddr lst)))
      nil))

(defun in_circle (x y cx cy radius)
  "Is a point inside a circle?"
  (<= (+ (* (- x cx) (- x cx))
         (* (- y cy) (- y cy)))
      (* radius radius)))

(defun random_vals (num vals seed)
  "Get num reproducible pseudorandom values, based on the seed."
  (cond
    ((> num 0)
     (let ((next (mod (+ (* 1531 seed) 919) 65535)))
       (random_vals (- num 1)
                    (cons next vals)
                    next)))
    (t vals)))

(defun random_2d (num radius seed)
    "Get list of 2D values clustered withing radius."
    (pairwise (mapcar (lambda (x) (mod x radius)) (random_vals (* num 2) NIL seed)))
)

(defun is_tile_water (x y)
  "Procedural function to check if a tile is land or ater on our island."
  (not (some (lambda (circle) (in_circle x y (+ 15 (car circle)) (+ 15 (cdr circle)) 9)) (random_2d 8 30 15)))
)

;; Actions system

(defun run-actions (actions world)
  "Action pipeline for the world. Apply the functions one by one to world."
  (if (null actions)
      world
      (run-actions (cdr actions)
                   (funcall (car actions) world))))

(defun add-message (message)
  "Returns a function which is applied to the world state, which sets the message."
  (lambda (world_env) (set_val 'message message world_env))
)

(defun set-player-pos (new-pos)
  "Returns a function which is applied to the world state, which sets the player pos."
  (lambda (world_env) (set_val 'player_pos new-pos world_env))
)


(defun move_player (dx dy world_env)
  "Move a player by the given delta."
  (let* ((pos (get_val 'player_pos world_env))
         (x (car pos))
         (y (cdr pos))
         (new_pos (cons (+ x dx)
                        (+ y dy))))
    (run-actions (list
      (set-player-pos new_pos)
      (add-message "You walk a short distance."))
     world_env)))

(defun lookup (key table)
  "Look up value in an assoc table"
  (cdr (assoc key table)
))

(defun is-tile-navigable (attribute)
  "Can the player walk on this attribute?"
  (lookup attribute '(  (forest . t)
                        (plains . t) 
                        (empty . t) 
                        (beach . t) 
)))


(defun move_to_if_free (x-delta y-delta world_env)
  (let* ((player_pos (get_val 'player_pos world_env)) (try-x (+ (car player_pos) x-delta)) (try-y (+ (cdr player_pos) y-delta)))
    (if (is-tile-navigable (tile_attributes try-x try-y world_env)) 
      (move_player x-delta y-delta world_env) 
      (funcall (add-message "You can't go in that direction.") world_env)))
  )


(defun parse_user_input (user_input world_env)
  (let ((first-char (char user_input 0)))
    (cond 
      ((char-equal first-char #\w) (move_to_if_free 0 -1 world_env)) 
      ((char-equal first-char #\s) (move_to_if_free 0 1 world_env)) 
      ((char-equal first-char #\a) (move_to_if_free -1 0 world_env)) 
      ((char-equal first-char #\d) (move_to_if_free 1 0 world_env))
      ((char-equal first-char #\h) (print world_env))
      (t world_env))))


(defun render_world (world_env)
    (let ((map_size (get_val 'map_size world_env)))
    (dotimes (y (car map_size))
        (dotimes (x (cdr map_size))
            (princ (lookup 
                (tile_attributes x y world_env) '(  (forest . ^)
                                                    (plains . _) 
                                                    (water . w) 
                                                    (empty . e) 
                                                    (beach . b) 
                                                    (cliff . c))
            )
            )
        )
        (terpri)
    )
    )
)

(defun describe_current_location (world_env)
    (let* ( (player_pos (get_val 'player_pos world_env)) 
            (tile (tile_attributes (car player_pos) (cdr player_pos) world_env)))
                (format nil "~{~a~}" 
                (list 
                (get_val 'message world_env)
                (cdr (assoc 
                    tile '(  (forest . "You are in a forest")
                            (plains . "You are on an open plain") 
                            (water . "water (OOB)") 
                            (empty . "empty") 
                            (beach . "You are standing on a beach") 
                            (cliff . "cliff (OOB)"))
                )) ". "
                "To the north " (describe_location_brief (cons (car player_pos) (- (cdr player_pos) 1)) world_env) ". "
                "To the east " (describe_location_brief (cons (+ (car player_pos) 1) (cdr player_pos)) world_env) ". "
                "To the south " (describe_location_brief (cons (car player_pos) (+ (cdr player_pos) 1)) world_env) ". "
                "To the west " (describe_location_brief (cons (- (car player_pos) 1) (cdr player_pos)) world_env) ". "
                )))
            )

(defun describe_location_brief (pos world_env)
    (let ((tile (tile_attributes (car pos) (cdr pos) world_env)))
            (cdr (assoc 
                tile '(  (forest . "is a forest")
                        (plains . "are open plains") 
                        (water . "are crashing waves") 
                        (empty . "is empty") 
                        (beach . "is a beach") 
                        (cliff . "are sheer cliffs"))
            ))))

(defun main_loop ()
  (let ((world (create_world_env)))
    (loop
      (format t "~a~%" (describe_current_location world))
      (format t "~&> ")
      (finish-output)  ;; ensure prompt is printed immediately
      (let ((user_input (read-line)))
        (when (string-equal user_input "quit")
          (return))
        (setf world (parse_user_input user_input world)) ;; set world to the new value
      ))))


(render_world (create_world_env))

(main_loop)

