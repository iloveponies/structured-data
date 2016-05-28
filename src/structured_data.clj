(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and
   (<= x1 px x2)
   (<= y1 py y2)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors  book))))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not  (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-sons (fn [coll] (second coll))]
    (map second-sons collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author) ]
    (if (contains? author :birth-year)
      (str name " (" (:birth-year author) " - " (:death-year author) ")")
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by "(authors->string (:authors book))))

(defn books->string [books]
  (let [apply2book (fn [book] (apply book->string book))]
    (cond
      (= 0 (count books))    "No books."
      (= 1 (count books))    (str "1 book. " (apply2book books) ".")
      :else                  (str (count books) " books. " (apply str (interpose ", " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book]
         (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [actual-author (filter (fn [author]
                                (= name (second (second author)))) authors)]
    (if (= 0 (count actual-author))
      nil
      (first actual-author))))

(defn living-authors [authors]
  (filter (fn [author]
            (alive? author)) authors))

(defn has-a-living-author? [book]
  (first (map alive? (:authors book))))

(defn books-by-living-authors [books]
  (filter (fn [book]
            (has-a-living-author? book)) books))

                                        ; %________%
