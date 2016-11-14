;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

;(defn width [rectangle]
;  :-)

(point 6 7)
(rectangle (point 6 7) (point 8 9))
(rectangle [6 7] [8 9])


(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- y2 y1)
    )
  )

(height (rectangle [6 7] [8 9]))

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- x2 x1)
    )
  )

(width (rectangle [6 7] [8 9]))

(defn square? [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (if (= (- x2 x1) (- y2 y1) )
      true
      false
    )
  )
)


(square? (rectangle [6 7] [8 9]))
(square? (rectangle [0 3] [8 9]))


(defn area [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (* (- x2 x1) (- y2 y1))
  )
)

(area (rectangle [0 3] [8 9]))

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2] ] rectangle
          [px py]  point]
    (if (and (<= x1 px x2) (<= y1 py y2))
      true
      false
      )
  )
)

(contains-point? (rectangle [0 0] [8 8]) (point 0 0))

;?bottom-left [0 0]
;                           ?top-right [2 2]
;                           ?point [1 1]
;                           ?contains? true]
(contains-point? (rectangle [0 0][2 2])(point 1 1))
(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))



(defn contains-rectangle? [outer inner]
  (let [ [[x1 y1] [x2 y2]] outer
         [[ix1 iy1] [ix2 iy2]] inner ]
      (if (and (contains-point? outer (point ix1 iy1))(contains-point? outer (point ix2 iy2)))
        true
        false
    )
  )
)

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2]))

(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3]))

(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2]))

(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1]))


















