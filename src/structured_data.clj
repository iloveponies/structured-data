(ns structured-data)

(defn do-a-thing [x]
  (let [s (+ x x)]
    (Math/pow s s)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
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
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp]           point
       ]
    (and (>= x2 xp x1) (>= y2 yp y1))
  )
) 

(defn contains-rectangle? [outer inner]
  (let [[[x1o y1o] [x2o y2o]] outer
        [[x1i y1i] [x2i y2i]] inner
       ]
    (and (>= x2o x2i x1i x1o) (>= y2o y2i y1i y1o))
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
  (let [authors (get book :authors)]
    (assoc book :authors (conj  authors new-author))
  )
)

(defn alive? [author]
  (not (:death-year author))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)
  )
) 

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)
  )
)

;(defn contains-duplicates? [a-seq]
;  (not (= (sort (seq (set a-seq)))  (sort a-seq)))
;)

(defn contains-duplicates? [a-seq]
  (not (= (count (seq (set a-seq))) (count a-seq)))
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
  (set ( map :name (authors books))) 
)

(defn author->string [author]
  (if (:birth-year author)
      (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")" )
      (str (:name author))
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
  (let [n (count books)]
    (cond
      (= n 0) "No books."
      (= n 1) (str "1 book. " (book->string (first books))".")
      :else (str n " books. " (apply str (interpose ". " (map book->string books))) ".")
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x)))  authors))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (not( empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
