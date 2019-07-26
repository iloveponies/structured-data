(ns structured-data)

(defn do-a-thing [x]
  (let [ex (+ x x)]
    (Math/pow ex ex)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y] [(first v) (nth v 2)]]
       (+ x y)))

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
 (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[p1 p2] point]
      (and (<= x1 p1 x2) (<= y1 p2 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[ix1 iy1] [ix2 iy2]] inner]
    (and (contains-point? outer [ix1 iy1]) (contains-point? outer [ix2 iy2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (let [new (assoc book :authors (conj new-author authors))]
      new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secondify (fn [x] (get x 1))]
    (map secondify collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [inc? (fn [x] (apply <= x))]
    (let [dec? (fn [x] (apply >= x))]
      (or (inc? a-seq) (dec? a-seq)))))

(defn stars [n]
  (apply str (concat (repeat n "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authz (fn [book] (:authors book))]
    (apply clojure.set/union (map authz books))))

;assign authors name to a function that gets name of each author in
;all authors output of books. Then calls that function on the input
;param (books), apply concat (apply the string concat to each
;successive item resulting from map and casts as a set to remove dups
(defn all-aothor-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)]
    (if (contains? author :birth-year)
      (let [date1 (:birth-year author)
            date2 (:death-year author)]
        (str name " (" date1 " - " date2 ")"))
      (str name)
      )))

(defn authors->string [authors]
  (apply str (interpose "," (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (< 0 (count books))
    (apply str (count books) " books." (interpose "," (map book->string books)))
    "No books."
    ))

(defn books-by-author [author books]
 (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
 (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
