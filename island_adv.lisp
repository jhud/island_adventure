

(defun create_world_env ()
  '((time . 0)
    (player . (10 . 29))
    (map_size . (32 . 32))))

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
  (if lst
      (cons (cons (first lst) (second lst))
            (pairwise (cddr lst)))
      nil))

(defun in_circle (x y cx cy radius)
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
    (not (some (lambda (circle) (in_circle x y (+ 8 (car circle)) (+ 9 (cdr circle)) 9)) (random_2d 9 14 25)))
)

(defun move_player(x_delta y_delta world_env)
    (let ((player_pos (get_val 'player world_env)))
        (set_val 'player (cons (+ (car player_pos) x_delta) 
                               (+ (cdr player_pos) y_delta)) world_env)
    )
)

(defun parse_user_input (user_input world_env)
    (let ((first_letter (car user_input)))
        (cond 
            ((eq first_letter 'w) (move_player 0 -1 world_env)) 
            ((eq first_letter 's) (move_player 0 1 world_env)) 
            ((eq first_letter 'a) (move_player -1 0 world_env)) 
            ((eq first_letter 'd) (move_player 1 0 world_env)) 
        )
    )
)

(defun render_world (world_env)
    (let ((map_size (get_val 'map_size world_env)))
    (dotimes (y (car map_size))
        (dotimes (x (cdr map_size))
            (princ (cdr (assoc 
                (tile_attributes x y world_env) '(  (forest . ^)
                                                    (plains . _) 
                                                    (water . w) 
                                                    (empty . e) 
                                                    (beach . b) 
                                                    (cliff . c))
            ))
            )
        )
        (terpri)
    )
    )
)

(defun main_loop ()
    (let ((world (create_world_env)))
        (loop
            (format t "~&> ")
            (let ((user_input (read-line)))
                (when (string-equal user_input "quit")
                    (return))
                (parse_user_input user_input world)
            )
        )
    )
)


(render_world (create_world_env))

(main_loop)

