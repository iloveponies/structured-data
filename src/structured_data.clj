(ns structured-data)

(defn do-a-thing [x]
  (let [xx (* x x)])
  (Math/pow (+ x x) (+ x x))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
   (let  [[[x1 x2] [y1 y2]] rectangle]
  (- y1 x1)))

(defn height [rectangle]
  (let  [[[x1 x2] [y1 y2]] rectangle]
  (- y2 x2)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let  [[[x1 x2] [y1 y2]] rectangle [z1 z2] point]
  (if (<= x1 z1 y1) (if (<= x2 z2 y2) true false)false)))

(defn contains-rectangle? [outer inner]
  (let  [[[x1 x2] [y1 y2]] outer [[z1 z2] [a1 a2]] inner]
    (if (contains-point? outer (point z1 z2)) (if (contains-point? outer (point a1 a2)) true false)false)
  ))

(defn title-length [book]
    (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [apu (fn [coll] (get coll 1))]
    (map apu collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (< (get a-seq 0) (get a-seq 1)) (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [x (count a-seq)
        y (count (set a-seq))]
    (if (== x y) false true)))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)))

(defn authors [books]
  (let [authors (map :authors books)]
    (apply clojure.set/union authors)))

(defn all-author-names [books]
  (let [author-names (map :name (authors books))]
    (set author-names)))

(defn author->string [author]
  (let [authorname (get author :name)
        by (get author :birth-year)
        dy (get author :death-year)]
    (cond
     dy (str authorname " (" by " - " dy ")")
     by (str authorname " (" by " - )")
     :else authorname)))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)
        authors (get book :authors) ]
    (str title ", written by "(authors->string authors))))

(defn books->string [books]
  (let [count (count books)]
    (cond
     (== count 1) (apply str (apply str count " book. " (interpose ". " (map book->string books))) ".")
     (> count 1) (apply str (apply str count " books. " (interpose ". " (map book->string books))) ".")
     :else "No books.")
    ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (let [a-name (get author :name)] (= name a-name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [authors (get book :authors)
        alive-ones (living-authors authors)]
    (> (count alive-ones) 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%





