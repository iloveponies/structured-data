(ns structured-data)

(defn do-a-thing [x]
  (let [x x] (Math/pow (+ x x) (+ x x))))

(defn spiff [v]
  (+ (get v 0)  (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]
   )


(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2  x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2  y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x2 x1) (- y2 y1)) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1) )))



(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (and
     (if (<= x1 (get point 0)  x2) true false)
     (if (<= y1  (get point 1) y2 ) true false)
     )))

(defn contains-rectangle? [outer inner]
 (let [[[xi1 yi1][xi2 yi2]] inner]
     (and
      (if(contains-point? outer (point xi1 yi1)) true false)
      (if(contains-point? outer (point xi2 yi2)) true false)
)))

(defn title-length [book]
  (count  (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))



(defn add-author [book new-author]
    (let
     [new  (conj (get book :authors) new-author)]
      (assoc book :authors new)))



(defn alive? [author]
  (if(contains? author :death-year) false true))



(defn element-lengths [collection]
  (map count collection))



(defn second-elements [collection]
  (let [toka (fn [x] (get x 1))]
    (map toka collection)))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
   (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
 (apply str(repeat n "*")) )

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count(set a-seq)) ) false true))

(defn old-book->new-book [book]

  (assoc book :authors  (set (:authors book))))


(defn has-author? [book author]
 (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books )))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
 (let [ moi (fn [x]
    (cond
     (:death-year x) (str " (" (:birth-year x) " - " (:death-year x) ")")
     (:birth-year x) (str " (" (:birth-year x) " - )")
     :else ""))]
  (str (:name author) (moi author))))

(defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str  (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
   (let [ x (count books) ]
   (cond
    (== x 0) "No books."
    (== x 1) (str "1 book. " (book->string (first (seq books)))".")
    :else (str x " books. " (apply str (interpose ". " (map book->string books)))"."))))
(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if(empty? (living-authors (:authors book)))false true))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

