(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)) )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn left [rectangle] (get (get rectangle 0) 0))
(defn bottom [rectangle] (get (get rectangle 0) 1))
(defn right [rectangle] (get (get rectangle 1) 0))
(defn top [rectangle] (get (get rectangle 1) 1))

(defn width [rectangle]
  (- (right rectangle) (left rectangle)))

(defn height [rectangle]
  (- (top rectangle) (bottom rectangle)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let
    [x (get point 0)
     y (get point 1)
  ] (and
        (<= x (right rectangle))
        (>= x (left rectangle))
        (<= y (top rectangle))
        (>= y (bottom rectangle)))))

(defn contains-rectangle? [outer inner]
  ( and
    (contains-point? outer (get inner 0))
    (contains-point? outer (get inner 1))
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [auth (get book :authors)]
    (assoc book :authors (conj auth new-author))))

(defn alive? [author]
  (= (get author :death-year) nil)
  )

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
  ))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  ((if
    (contains? a-set elem)
    disj
    conj) a-set elem)
  )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)) ))

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (let [authors-seq (map :authors books)]
    (apply clojure.set/union authors-seq)
  )
)

(defn all-author-names [books]
  (let [author-namess-seq (map :name (authors books))]
    (set author-namess-seq)
  )
)

(defn author->string [author]
  (let [year (if
            (:birth-year author)
            (str " (" (:birth-year author) " - " (:death-year author) ")")
            ""
            )]
    (str (:name author) year)
  )

  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [
    book-string-and-dot (fn [book] (str " " (book->string book) "."))
    c (count books)
    books-string (if (= c 1) "book" "books")]
  (if (= c 0)
    (str "No " books-string "." )
    (str c " " books-string "." (apply str (map book-string-and-dot books))))
  )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  ( let [name-equals (fn [author] (= (:name author) name))]
  (first (filter name-equals authors))
  )
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
