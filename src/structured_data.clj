(ns structured-data)

(defn do-a-thing [x]
  (let [xxx (+ x x)]
    (Math/pow xxx xxx)))


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
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))


(defn height [rectangle]
  (let[[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
      true
      false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle]
    (let [[x3 y3] point]
     (and (<= x1 x3 x2) (<= y1 y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [a (get book :authors)]
    (assoc book :authors (conj a new-author)
    )))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (seq (map count collection)))

(defn second-elements [collection]
  (let [second-items (fn [colle] (get colle 1))]
    (seq(map second-items collection))))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names (fn [book] (:authors book))]
    (apply clojure.set/union (map author-names books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)]
    (let [birth (:birth-year author)]
      (let [death (:death-year author)]
        (cond
         (boolean birth) (str author-name " (" birth " - " death ")" )
         :else (str author-name))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
   (== (count books) 1) (str (count books) " book. " (apply str (interpose ". " (map book->string books))) ".")
   :else "No books."))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author )) books ))

(defn author-by-name [name authors]
  (let [aauth (fn [a] (=(:name a) name)) ]
    (cond
     (= (count (filter aauth authors)) 0) nil
     :else (first (filter aauth authors)))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
