(ns structured-data)

(defn do-a-thing [x]
 (let [sum (+ x x)]
   (Math/pow sum sum)))

(defn spiff [v]
  (if (>= (count v) 3)
  (+ (get v 0) (get v 2))
  nil)
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (if (= nil c)
      nil
    (+ a c))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
 (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
 (let [[[x1 y1] [x2 y2]] rectangle]
   (- y2 y1)))

(defn square? [rectangle]
  (let [r rectangle] 
    (= (height r) (width r))))

(defn area [rectangle]
  (let [r rectangle] 
    (* (height r) (width r))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (and (>= x x1) (<= x x2)) (and (>= y y1) (<= y y2)))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[innerbottom innertop] inner]
    (and (contains-point? outer innerbottom) (contains-point? outer innertop))
    )
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (do (println book)
  (println new-author))
  (let [exauth (book :authors)]
  (assoc book :authors (conj exauth new-author))
  ))

(defn alive? [author]
  (= (author :death-year) nil))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [fun #(if (> (count %) 1) (nth % 1) nil)]
    (map fun collection))
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
  )

(defn has-author? [book author]
  (contains? (book :authors) author)
)

(defn authors [books]
  (let [union clojure.set/union]
  (apply union (map :authors books))
  )
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (str (:name author) 
       (if (:birth-year author) (str " (" (:birth-year author) " - " (:death-year author) ")")))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn bookcount[books]
 (cond 
  (= 0 (count books)) "No books"
  (= 1 (count books)) "1 book. "
  :else (str (count books) " books. ") 
  ))

(defn books->string [books]
  (str (bookcount books)
     (apply str (interpose ". " (map book->string books)))
     "."
   )  
)


(defn books-by-author [author books]
  (filter #(has-author? % author) books)
  )

(defn author-by-name [name authors]
  (first (filter #(= name (% :name)) authors))
  )

(defn living-authors [authors]
  (filter #(alive? %) authors)
  )


(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors))))
  )

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books)
  )

; %________%
