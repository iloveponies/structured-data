(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
     (Math/pow xx xx)))

(defn spiff [v]
  (cond
     (== (count v) 1) (get v 0)
     (== (count v) 0) nil)
     :else (+ (get v 0) (get v 2)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (height rectangle) (width rectangle))
      true
      false)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
      [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner
        point1 [x1 y1]
        point2 [x2 y2]]
    (if (and (contains-point? outer point1) (contains-point? outer point2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [add-writer (conj (book :authors) new-author)]
    (assoc book :authors add-writer)))

(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [element2 (fn [coll] (second coll))]
    (map element2 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
     (apply >= a-seq) true
     (apply <= a-seq) true
     :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
        (disj a-set elem)
        (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (= (count a-set) (count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (clojure.set/union (set (apply concat (map :authors books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (str (:name author))
        years (str " ("
                   (:birth-year author)
                   " - "
                   (:death-year author)
                   ")")]
    (if (:birth-year author)
      (str author-name years)
      (str author-name))))

(defn authors->string [authors]
  (cond
   (> (count authors) 1) (apply str (interpose ", " (map author->string authors)))
   (= (count authors) 1) (author->string (first authors))
   :else ""))

(defn book->string [book]
  (let [author-names (authors->string (:authors book))]
   (str (:title book) ", written by " author-names)))

(defn books->string [books]
  (let [books-seq (map book->string books)]
   (cond
     (empty? books) "No books."
     (= (count books) 1) (str "1 book. " (apply str books-seq) ".")
     :else (str (count books) " books. " (apply str (interpose ". " books-seq)) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (get (map :name (filter first authors)) name))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (living-authors (:authors book))]
        (not (empty? authors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
