; Exercise 30
(ns structured-data)

(defn do-a-thing [x]
  (let [doubled] (+ x x)
    (Math/pow doubled doubled)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn abs [x]
 (if (< x 0) (- x) x))


(defn width [[[width1] [width2]]]
  (abs (- width1 width2)) 
)

(defn height [[[_ height1] [_ height2]]]
  (abs (- height1 height2))
)

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
)

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
)

(defn contains-point? [rectangle [point1, point2]]
 (and (<= point1 (width rectangle)) (<= point2 (height rectangle))) 
)

(defn contains-rectangle? [outer [inner1, inner2]]
  (and (contains-point? outer inner1) (contains-point? outer inner2))
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (< 1 (count (:authors book)))
)

(defn add-author [book new-author]
  (let [updated-authors
    (conj (get book :authors) new-author)]
    (assoc book :authors updated-authors)
  )
)

(defn alive? [author]
  (not (get author :death-year))
)

(defn element-lengths [collection]
  (let [count-elements (fn [c] (count c))]
    (map count-elements collection)
  )
)

(defn second-elements [collection]
  (let [second-element (fn [c] (get c 1))]
    (map second-element collection)
  )
)

(defn titles [books]
 (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
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

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq))))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (set (map :authors books))
)

(defn all-author-names [books]
 (set (map :name (apply clojure.set/union (authors books))))
)

(defn author->string [author]
  (let
    [lifespan (if (:birth-year author) (str " (" (:birth-year author) " - " (:death-year author) ")") nil)]
    (str (:name author) lifespan)
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [authors-string (authors->string (:authors book))]
    (str (:title book) ", written by " authors-string)
  )
)

(defn books->string [books]
  (let [number-of-books (count books)]
    (cond
      (= number-of-books 0) "No books."
      (= number-of-books 1) (str "1 book. " (apply str (interpose ". " (map book->string books))))
      :else (str number-of-books " books. " (apply str (interpose ". " (map book->string books))) ".")
    )
  )
)

(defn books-by-author [author books]
  (remove nil? (map (fn [book] (if (has-author? book author) book nil)) books)))

(defn author-by-name [name authors]
  (let [contains-name? (fn [author] (if (= (:name author) name) author nil))]
    (first (remove nil? (map contains-name? authors)))
  )
)

(defn living-authors [authors]
  (let [ alive-author (fn [author] (if (alive? author) author nil)) ]
   (remove nil? (map alive-author authors))
  )
)

(defn has-a-living-author? [book]
  (not (empty? (map living-authors (:authors book))))
)

(defn authors-or-nil [book]
  (if (not (empty? (living-authors (:authors book))))
    book
    nil
  )
)

(defn books-by-living-authors [books]
 (remove nil? (map authors-or-nil books))
)

; %________%
