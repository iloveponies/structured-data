(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (if (== (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (if (and (<= x1 px x2) (<= y1 py y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if (and (contains-point? outer p1) (contains-point? outer p2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-author-list (conj (:authors book) new-author)
        oldtitle (:title book)]
    (if (contains? book :title)
    {:title oldtitle :authors new-author-list}
      {:authors new-author-list})))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [x] (get x 1))]
    (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
  (assoc book :authors authors-set)))


(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
 (let [helper (fn [book] (:authors book))]
  (apply clojure.set/union (map helper books))))

(defn all-author-names [books]
  (let [helper (fn [x] (:name x))]
    (set (map helper (authors books)))))

(defn author->string [author]
  (cond (contains? author :death-year) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
        (contains? author :birth-year) (str (:name author) " (" (:birth-year author) " - )")
        :else (:name author)))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
 (let [authors-string (authors->string (:authors book))]
  (str (:title book) ", written by " authors-string)))

(defn books->string [books]
  (let [book-count (count books)]
    (cond (== book-count 0) "No books."
          (== book-count 1) (str book-count " book. " (book->string (first books)) ".")
          :else (str book-count " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [helper (fn [book] (has-author? book author))]
    (filter helper books)))

(defn author-by-name [name authors]
  (let [helper (fn [x] (= name (:name x)))
        result (filter helper authors)]
    (if (== 0 (count result))
      nil
      (first result))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
 (not
  (empty?
   (living-authors
    (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
