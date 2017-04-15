(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
 (Math/pow xx xx))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (- (get (second rectangle) 0) (get (first rectangle) 0))
  )

(defn height [rectangle]
  (- (get (second rectangle) 1) (get (first rectangle) 1))
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (and
   (<= (get (first rectangle) 0) (get point 0)) true
   (<= (get (first rectangle) 1) (get point 1)) true
   (>= (get (second rectangle) 0) (get point 0)) true
   (>= (get (second rectangle) 1) (get point 1)) true
   )
  )

(defn contains-rectangle? [outer inner]
  (and
   (contains-point? outer (first inner))
   (contains-point? outer (second inner))
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
  (let [new-authors (conj (:authors book) new-author)]
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
  (let [get-second (fn [x] (get x 1))]
     (map get-second collection ))
  )

(defn titles [books]
  (let [get-title (fn [x] (:title x))]
    (map get-title books)
  ))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)(apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   (not (contains? a-set elem)) (conj a-set elem)
   ))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (book :authors) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (fn [] (:name author))
        birth (fn [] (:birth-year author))
        death (fn [] (:death-year author))]
    (cond
     (death) (str (name) " (" (birth) " - " (death)")")
     (birth) (str (name) " (" (birth) " - )")
     :else (str (name))
     )
   )
  )

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors)))
  )

(defn book->string [book]
  (let [title (fn [] (:title book))
        authors (fn [] (:authors book))]
    (str (title) ", written by " (authors->string (authors)))
    )
  )

(defn books->string [books]
  (cond
   (= (count books) 0) (str "No books.")
   (= (count books) 1) (str "1 book. " (apply str (map book->string books))".")
   :else (str (count books) " books. " (apply str (interpose ", " (map book->string books)))".")
   )
  )

(defn books-by-author [author books]
  (let [has-author (fn [x] (has-author? x author))]
   (filter has-author books))
  )

(defn author-by-name [name authors]
  (let [name-found (fn [x] (= name (:name x)))]
    (first (filter name-found authors))
 ))

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0)
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%







