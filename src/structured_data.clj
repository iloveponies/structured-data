(ns structured-data)

(defn do-a-thing [x]
  (let [z (+ x x)]
  (Math/pow z z)))

(defn spiff [v]
  (if (> (count v) 2)
    (+ (get v 0) (get v 2))
    "?"))

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)
    ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))
  ))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner)) (contains-point? outer (second inner))
    ))

(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1) true false
    )
)


(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))


(defn alive? [author]
  (if (author :death-year) false true
    ))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getSecond (fn [x] (second x))]
    (map second collection)
    ))



(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false
    ))

(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)  (disj a-set elem) (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (let [x (count a-seq)]
    (if (< (count (set a-seq)) x) true false)
    ))

(defn old-book->new-book [book]
  (let [new (set (:authors book))]
    (assoc book :authors new)
    ))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false
    ))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        by (:birth-year author)
        dy (:death-year author)]
    (if (or by dy)
      (apply str [name, " (", by, " - ", dy, ")"]) (str name)
    )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (apply str [(:title book), ", written by ", (authors->string (:authors book))]
         ))

(defn books->string [books]
  (let [bookCount (count books)]
    (if (= bookCount 0) "No books."
      (str bookCount (if (= bookCount 1) " book. " " books. ")
           (apply str [(apply str (interpose ". " (map book->string books))), "."])))
    ))


(defn books-by-author [author books]
  (let [check (fn [book] (has-author? book author))]
          (filter check books)))

(defn author-by-name [name authors]
  (let [name-test (fn [x] (if (= (:name x) name) true false))]
    (if (empty? (filter name-test authors)) nil (first (filter name-test authors)))))

(defn living-authors [authors]
  (let [has-died (fn [author] (if (:death-year author) false true))]
    (filter has-died authors)))


(defn has-a-living-author? [book]
  (let [has-died (fn [author] (if (:death-year author) false true))
        authors (:authors book)
        filt (filter has-died authors)]
    (if (> (count filt) 0) true false))
)


(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))


; %________%
