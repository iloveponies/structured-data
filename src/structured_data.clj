(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))


(defn spiff [v]
  (let [length (count v)]
    (if (>= length 3) (+ (get v 0) (get v 2)) 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (if (and first third) (+ first third) 0)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[bottom-left top-right] rectangle]
    (let [[min-x min-y] bottom-left
          [max-x max-y] top-right]
      (- max-x min-x))))

(defn height [rectangle]
  (let [[bottom-left top-right] rectangle]
    (let [[min-x min-y] bottom-left
          [max-x max-y] top-right]
      (- max-y min-y))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[bottom-left top-right] rectangle]
    (let [[min-x min-y] bottom-left
          [max-x max-y] top-right
          [x y] point]
      (and (<= min-x x max-x)
           (<= min-y y max-y)))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        length (count authors)]
    (assoc book :authors (assoc authors length new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [take-second (fn [v] (get v 1))]
    (map take-second collection)))

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
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [lifespan (if (contains? author :birth-year)
                   (str " (" (:birth-year author) " - " (:death-year author) ")")
                   "")]
    (str (:name author) lifespan)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [length (count books)
        books-string (apply str (interpose ". " (map book->string books)))]
    (cond
       (== length 0) "No books."
       (== length 1) (str "1 book. " books-string ".")
       :else (str length " books. " books-string "." ))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
