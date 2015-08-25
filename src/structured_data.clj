(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[fst _ trd]]
    (+ fst trd))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x _] [y _]] rectangle]
    (- y x)))

(defn height [rectangle]
  (let [[[_ x] [_ y]] rectangle]
    (- y x)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [z1 z2]           point]
      (and (<= x1 z1 x2)
           (<= y1 z2 y2))))

(defn contains-rectangle? [outer inner]
  (let [ [i1 i2] inner]
    (and (contains-point? outer i1)
         (contains-point? outer i2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors     (:authors book)
        new-authors (conj authors
                          new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [cs] (get cs 1))]
    (map second collection)))

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
  (let [distinct (count (set a-seq))]
    (> (count a-seq) distinct)))

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
  (cond
    (contains? author :death-year)
      (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (contains? author :birth-year)
      (str (:name author) " (" (:birth-year author) " - )")
    :else
      (str (:name author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)]
    (cond
      (== n 0)
        "No books."
      (== n 1)
        (str "1 book. " (apply str (map book->string books)) ".")
      :else
        (str n " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [author?
          (fn [book] (contains? (:authors book) author))]
    (filter author? books)))

(defn author-by-name [name authors]
  (let [name? (fn [author] (= (:name author) name))]
    (first (filter name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
