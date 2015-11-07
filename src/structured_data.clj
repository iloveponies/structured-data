(ns structured-data)

(defn do-a-thing [x]
    (let [xx (+ x x)]
      (Math/pow xx xx)
      ))

(defn spiff [v]
  ( + (get v 0) (get v 2) ))

(defn cutify [v]
  (conj v "<3") )

(defn spiff-destructuring [v]
  (let [[a b c] v ]
    ( + a c ) )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn distance [a b]
  (if ( > (- a b) 0 )
    (- a b)
    ( * -1 (- a b) ) ))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle ]
    (distance x1 x2) )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle ]
    (distance y1 y2)
  )
)

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle ]
    (if ( == (distance x1 x2) (distance y1 y2) ) true false )
  ))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle ]
   (* (distance x1 x2) (distance y1 y2) )
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle ]
    (let [[px py] point ]
      (if (and ( <= x1 px x2 ) ( <= y1 py y2 )) true false) ))
)

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if (and (contains-point? outer p1) (contains-point? outer p2) ) true false)
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
