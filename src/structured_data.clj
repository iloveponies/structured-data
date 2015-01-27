(ns structured-data)

(defn do-a-thing [x]
 (let [sum (+ x x) ]
   (Math/pow sum sum)
 )
)

(defn spiff [v]
  (+ (get v 0) (get v 2) )
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [ [f s t] v ]
    (+ f t)
  )
)

(defn point [x y]
  [x y] )

(defn rectangle [bottom-left top-right]
  [bottom-left top-right] )

(defn width [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (- x2 x1)
  )
)

(defn height [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (- y2 y1)
  )
)

(defn square? [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (= (- x2 x1) (- y2 y1) )   
  )
)

(defn area [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (* (- x2 x1) (- y2 y1) )
  )
)

(defn contains-point? [rectangle point]
  (let [ [ [x1 y1] [x2 y2] ] rectangle 
                       [x3 y3] point ]
    
    (and (<= x1 x3 x2) (<= y1 y3 y2) )
  )
)

(defn contains-rectangle? [outer inner]
  (let [ [ first second ] inner ]
    (and (contains-point? outer first) (contains-point? outer second) )
  )
)

(defn title-length [book]
 (count (:title book) )
)

(defn author-count [book]
 (count (:authors book) )
)

(defn multiple-authors? [book]
 (if (> (author-count book) 1)
   true
   false
 )
)

(defn add-author [book new-author]
  (let [new_authors (conj (:authors book) new-author) ]
  
    (assoc book :authors new_authors)  
  )
)

(defn alive? [author]
  (not (contains? author :death-year) )
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [seconds (fn [set] (get set 1) ) ]
    (map seconds collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq) )
)

(defn stars [n]
  (apply str (repeat n \*) )
)

(defn toggle [a-set elem]

  (if (= a-set

)

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

; %________%<
