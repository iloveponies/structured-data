(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

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
    (if (== (height rectangle) (width rectangle))
      true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point] 
  (if (and (<= x1 x x2) (<= y1 y y2)) true false)))

(defn contains-rectangle? [outer inner]
  (if (let [[p1 p2] inner]
        (and (contains-point? outer p1) (contains-point? outer p2)))
    true false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]
  (let [v (:authors book) vnew (conj v new-author)] (assoc book :authors vnew)))

(defn alive? [author]
  (if (:death-year author) false true))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (>= (first a-seq) (second a-seq))  (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (clojure.string/join (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
   (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books] 
  (set  (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (:birth-year author)
                (str " (" (:birth-year author) " - "
                        (:death-year author) ")") nil)]
    (str name  years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str [(:title book) ", written by " (authors->string (:authors book))]))

(defn books->string [books]
  (let [bookcount (count books)
        rest (map #(str (book->string %) ".") books) ]
    (apply str  [(case bookcount
                  0 "No books."
                  1 "1 book. "
                  (apply str [bookcount " books. "]))
                 (apply str (interpose " " rest)
                   )])))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books) )

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (let [living (living-authors (:authors book))]
    (if (empty? living) false true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
