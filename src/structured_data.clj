(ns structured-data)

(defn do-a-thing
  [x]
  (let [xplus (+ x x)]
  (Math/pow xplus xplus)))

 (defn spiff
   [v]
   (+ (get v 0 0) (get v 2 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring
  [v]
  (let [[x b c] v]
   (+ x c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square?
  [rectangle]
    (= (width rectangle) (height rectangle)))

(defn area
  [rectangle]
    (* (width rectangle) (height rectangle)))

(defn contains-point?
  [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and
     (<= xp x2)
     (>= xp x1)
     (<= yp y2)
     (>= yp y1))))

(defn contains-rectangle?
  [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))));returns true if inner is inside outer.

(defn title-length
  [book]
  (count (get book :title)))

(defn author-count
  [book]
  (count (get book :authors)))

(defn multiple-authors?
  [book]
  (> (author-count book) 1))

(defn add-author
  [book new-author]
  (assoc book :authors (conj (book :authors) new-author)))

(defn alive? [author]
  (not (number? (get author :death-year))))

(defn element-lengths
  [collection]
  (map count collection))

(defn second-elements
  [collection]
  (let [getsec (fn [coll] (get coll 1))]
   (map getsec collection)))

(defn titles
  [books]
  (map :title books))

(defn monotonic?
  [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars
  [n]
  (apply str (repeat n "*")))

(defn toggle
  [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates?
  [a-seq]
  (not (=
   (count a-seq)
   (count (set a-seq)))))

 (defn old-book->new-book
   [book]
   (assoc book :authors (set (:authors book))))

(defn has-author?
  [book author]
  (contains? (:authors book) author))

(defn authors
  [books]
  (let [author-names
         (fn [book] (:authors book))]
    (apply clojure.set/union (map author-names books))))

(defn all-author-names
  [books]
  (set(map :name (authors books))))

(defn author->string
  [author]
   (if
     (contains? author  :birth-year)
      (str (:name author)'" (" (get author :birth-year) '" - "(get author :death-year) '")")
      (str (:name author))))

(defn authors->string
  [authors]
   (apply str (interpose ", " (map author->string authors))))

(defn book->string
  [book]
  (str (:title book) ", written by "  (authors->string (:authors book))))

(defn books->string
  [books]
  (cond (= (count books) 0) "No books."
        (= (count books) 1) (str "1 book. " (apply book->string books) ".")
        :else (str (count books) " books. " (apply str (interpose ". " (map book->string books)))  ".")))

(defn books-by-author
  [author books]
  (let [helper-author
        (fn [booksColl] (has-author? booksColl author))]
   (filter helper-author books)))

(defn author-by-name
  [name authors]
  (let [helper-name
         (fn [authorsColl] (= (:name authorsColl) name))]
   (first (filter helper-name authors))))

(defn living-authors
  [authors]
  (filter alive? authors))

(defn has-a-living-author?
  [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors
  [books]
  (filter has-a-living-author? books))

; %________%
