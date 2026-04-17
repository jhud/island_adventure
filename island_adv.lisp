

(defun make_world ()
  '((time . 0)
    (player . ((x . 0) (y . 0)))
    (map_size . (4 . 4))))

(defun get_val (key alist)
  (cdr (assoc key alist)))

(defun set_val (key value alist)
  (cons (cons key value)
        (remove key alist :key #'car)))

(defun get_player_pos (world)
  (get_val 'player world))

;; Get tile attributes at the given coordinates. Lisp does not really need enums, so we use symbols
(defun tile_attributes (x y)
    "Get the tile attributes at the given coordinates"
    (cond 
        ((< x 2) 'forest)
        ((< y 2) 'plains)
        ('t 'water)
    )
)


(defun parse_user_input (user_input)
    (let (first_letter (car user_input))
        (cond ((eq first_letter 'n) ) ; change x/y
        
        )
    )
)

(defun render_world ()
    (dotimes (x 4)
        (dotimes (y 4)
            (princ (cdr (assoc 
                (tile_attributes x y) '((forest . ^) (plains . _) (water . w))
            ))
            )
        )
        (terpri)
    )
)

(defun main_loop ()
    (let ((user_input (read-line)))
        (parse_user_input user_input)
    )
)

(make_world)

(render_world)

(main_loop)

