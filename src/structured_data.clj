(ns structured-data)

(defn do-a-thing [x]
  (let[z (+ x x)]
    (Math/pow z z)
  
  )
)
(defn spiff [v]
  (+ (get v 0) (get v 2)
  )
)

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
   )
  
  )

(defn height [rectangle]
  
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
  
  )
)

(defn square? [rectangle]
  
  (if (= (width rectangle) (height rectangle))
    
    true

    false
    
    )
  
  )

(defn area [rectangle]

  (* (width rectangle) (height rectangle))  
  
)

(defn contains-point? [rectangle point]
  
  (let [[[x1 y1][x2 y2]] rectangle [x3 y3] point]
    (and(<= x1 x3 x2)(<= y1 y3 y2))

   )
  
  )

(defn contains-rectangle? [outer inner]

  
  (let [[[x1 y1][x2 y2]] inner]
    (and(contains-point? outer [x1 y1])(contains-point? outer [x2 y2]))

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
