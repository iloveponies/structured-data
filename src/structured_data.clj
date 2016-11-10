(ns structured-data)

(defn do-a-thing [x]
  (let [num (+ x x) ]
    (Math/pow num num))
)

(defn spiff [v]
  (let [first (get v 0) third (get v 2) ]
    (+ first third) )
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y z] v ]
   (+ x z))
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (if (= (- x2 x1) (- y2 y1) )
      true
      false
    )
  )
)

(defn area [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (* (- x2 x1) (- y2 y1))
  )
)

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2] ] rectangle
          [px py]  point]
    (if (and (<= x1 px x2) (<= y1 py y2))
      true
      false
      )
  )
)

(defn contains-rectangle? [outer inner]
  (let [ [[x1 y1] [x2 y2]] outer
         [[ix1 iy1] [ix2 iy2]] inner ]
      (if (and (contains-point? outer (point ix1 iy1))(contains-point? outer (point ix2 iy2)))
        true
        false
    )
  )
)

(defn title-length [book]
  :-)

(defn author-count [book]
  :-)

(defn multiple-authors? [book]
  :-)

(defn add-author [book new-author]
  :-)

(defn alive? [author]
  :-)

(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
