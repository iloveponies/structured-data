(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (let [[[x1] [x2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x1 x2) (- y1 y2))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x1 x2) (- y1 y2))))

(defn contains-point? [rectangle point]
  (let [[x y] point]
    (let [[[x1 y1] [x2 y2]] rectangle]
      (and (<= x1 x x2) (<= y1 y y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x2 y2)) (contains-point? outer (point x1 y1)))))


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original (get book :authors)
        newAuthors (conj original new-author)]
    (assoc book :authors newAuthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn m [x]
  (+ (count x)))

(defn element-lengths [collection]
  (map m collection))

(defn second-elements [collection]
  (let [helper (fn [v] (get v 1))]
    (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (let [v (repeat n "*")]
    (apply str v)))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(== (count a-seq) (count (into #{} a-seq)))))

(defn old-book->new-book [book]
  (let [authors (set (get book :authors))]
  (assoc book :authors authors)))


(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [authorNames (fn [book] (set (get book :authors)))]
    (set (apply clojure.set/union (map authorNames books)))))

(defn all-author-names [books]
  (let [auth (authors books)] (set (map :name auth))))


(defn author->string [author]
  (let [name (get author :name)]
    (cond
      (contains? author :death-year) (str name " (" (get author :birth-year) " - " (get author :death-year) ")")
      (contains? author :birth-year) (str name " (" (get author :birth-year) " - )")
      :else name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors (get book :authors)] (str (get book :title) ", written by " (authors->string authors))))

(defn books->string [books]
  (let [booksCount (count books)] booksCount
   (cond
    (empty? books) "No books."
    (== booksCount 1) (str (apply str "1 book. " (interpose ", " (map book->string books))) ".")
    :else (str (apply str booksCount " books. " (interpose ", " (map book->string books))) "."))))


(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [names
  (filter (fn [x] (= (get x :name) name)) authors)]
    (cond
     (empty? names) nil
     :else (first names))))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (let [livingAuthors (living-authors (get book :authors))]
    (> (count livingAuthors) 0)))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
