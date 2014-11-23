(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v1 v2 v3] v]
    (+ v1 v3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- x1 x0)))

(defn height [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- y1 y0)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle
        [x y] point]
    (and (<= x0 x x1)
         (<= y0 y y1))))

(defn contains-rectangle? [outer inner]
  (let [[inner0 inner1] inner]
    (and (contains-point? outer inner0)
         (contains-point? outer inner1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author)
       (cond
         (:death-year author) (str " (" (:birth-year author) " - " (:death-year author) ")")
         (:birth-year author) (str " (" (:birth-year author) " - )"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (str
    (cond
      (== (count books) 0) "No books"
      (== (count books) 1) "1 book. "
      :else (str (count books) " books. "))
    (apply str (interpose ". " (map book->string books)))
   "."))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
