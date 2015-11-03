(ns structured-data)

(defn do-a-thing [x]
  (let [XX (+ x x) ]
       (Math/pow XX XX)
  ))

(defn oInt [x]
  (if (= x  nil) 0 x)
)

(defn spiff [v]
     (+ (oInt(get v 0)) (oInt(get v 2)))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [[a b c]]
    (+ (oInt a) (oInt c))
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  )
)

(defn height [rectangle]
 (let [ [[x1 y1] [x2 y2]] rectangle]
     (- y2 y1)
  )
)

(defn square? [rectangle]
    (= (width rectangle) (height rectangle))
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
    (let [ [x y] point
           [ [x1 y1] [x2 y2] ]  rectangle ]
      (and (<= x1 x x2) (<= y1 y y2))
    )
)

(defn contains-rectangle? [outer inner]
  (let [ [bottom-left top-right] inner ]
    (and  (contains-point? outer bottom-left)
          (contains-point? outer top-right))
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (> (author-count book) 1)
)

(defn add-author [book new-author]
  (let  [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [ sec-el (fn [col] (get col 1)) ]
  (map sec-el collection)
  )
)

(defn titles [books]
 (map :title books)
  )

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)
  )
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq)))
)

(defn old-book->new-book [book]

  (assoc book :authors (set (:authors book)))

)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
    (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
   (set (map :name (authors books)))
)

(defn author->string [author]
 (cond
     (contains? author :death-year) (apply str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
     (contains? author :birth-year) (apply str (:name author) " (" (:birth-year author) " - )")
     :else (str (:name author))
  )
)


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (= (count books) 1) (str "1 book. " (book->string (get books 0)) ".")
    :else (str (count books) " books. " (apply str (map (fn [book] (str (book->string book) ". ")) books) ) )
  )
)

(defn books-by-author [author books]
  (let [ by-this-author (fn [book] (has-author? book author))]
    (filter  by-this-author  books)
  )
)

(defn author-by-name [name authors]
  (let [has-this-name (fn [author]  (= (:name author) name))]
    (first (filter has-this-name authors) )
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
