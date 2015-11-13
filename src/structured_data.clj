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
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if ( > (author-count book) 1 ) true false ))

(defn add-author [book new-author]
  (let [newAuthors (conj (:authors book) new-author)]
    (assoc book :authors newAuthors)))

(defn alive? [author]
  (if (:death-year author) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [collec] (get collec 1))]
    (map get-second collection ) ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or ( apply >= a-seq ) ( apply <= a-seq )) true false ))

(defn stars [n]
  (apply str (repeat n "*") ) )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem) ))

(defn contains-duplicates? [a-seq]
  (if ( == (count a-seq) (count (set a-seq) ) )
    false
    true ))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors) )))

(defn has-author? [book author]
  (if ( contains? (book :authors) author ) true false ) )

(defn authors [books]
   (set ( apply concat (map :authors books) ) ))

(defn all-author-names [books]
  (set (map :name (authors books)) ))

(defn author->string [author]
  (let [name (:name author )
        years (str " (" (:birth-year author) " - " (:death-year author) ")" ) ]
    (if (:birth-year author)
      (str name years)
      (str name) )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)) ))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book) ) ))

(defn books->string [books]
  (let [books-string (apply str (interpose ". " (map book->string books )))]
    (cond
     (empty? books) (str "No books.")
     (== (count books) 1 ) (str "1 book. " books-string "." )
     (> (count books) 1 )  (str (count books) " books. " books-string "." )
      )))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books) )

(defn author-by-name [name authors]
  (let [found-authors (filter (fn [x] (= (:name x) name)) authors)]
  (if (empty? found-authors)
    nil
    (first found-authors )) ))

(defn living-authors [authors]
  (filter alive? authors) )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))) ))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books) )

; %________%
