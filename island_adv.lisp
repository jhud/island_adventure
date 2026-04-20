

(defun world_env ()
  '((time . 0)
    (player . ((x . 10) (y . 29)))
    (map_size . (32 . 32))))

(defun get_val (key alist)
  (cdr (assoc key alist)))

(defun set_val (key value alist)
  (cons (cons key value)
        (remove key alist :key #'car)))

(defun get_player_pos (world)
  (get_val 'player world))

;; Get tile attributes at the given coordinates. Lisp does not really need enums, so we use symbols
(defun tile_attributes (x y world_env)
    "Get the tile attributes at the given coordinates"
    (let ((map_size (get_val 'map_size world_env)))
    (cond 
        ((is_tile_water x y) 'water)
        ((< x (/ (car map_size) 1)) 'forest)
        ((< y (/ (cdr map_size) 1)) 'plains)
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
  (cond
    ((> num 0)
     (let ((next (mod (+ (* 1531 seed) 919) 65535)))
       (random_vals (- num 1)
                    (cons next vals)
                    next)))
    (t vals)))

(defun random_2d (num radius seed)
    (pairwise (mapcar (lambda (x) (mod x radius)) (random_vals (* num 2) NIL seed)))
)

(defun is_tile_water (x y)
    (not (some (lambda (circle) (in_circle x y (+ 8 (car circle)) (+ 9 (cdr circle)) 8)) (random_2d 9 14 25)))
)

(defun parse_user_input (user_input)
    (let (first_letter (car user_input))
        (cond ((eq first_letter 'w) ) ; change x/y
        
        )
    )
)

(defun render_world (world_env)
    (let ((map_size (get_val 'map_size world_env)))
    (dotimes (x (car map_size))
        (dotimes (y (cdr map_size))
            (princ (cdr (assoc 
                (tile_attributes x y world_env) '((forest . ^) (plains . _) (water . w) (empty . e))
            ))
            )
        )
        (terpri)
    )
    )
)

(defun main_loop ()
    (let ((user_input (read-line)))
        (parse_user_input user_input)
    )
)


(render_world (make_world))

(main_loop)

