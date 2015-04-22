(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))        ;=> ?

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
  (if (> (count (:authors book)) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))


(defn alive? [author]
  (if (:death-year author)
    false
    true));


(defn element-lengths [collection]
  (let [length (fn [x] (count x))]
    (map length collection)))

(defn second-elements [collection]
 (let [sec (fn [x] (get x 1))]
    (map sec collection)))

(defn titles [books]
   (map :title books))

(defn monotonic? [a-seq]
  (let [[a b] a-seq]
  (if (<= a b)
    (apply <= a-seq)
    (apply >= a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq))
    true
    false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))



(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))



(defn authors [books]
    (set (apply concat (map :authors books))))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author)
  (if (:birth-year author)
    (str " (" (:birth-year author) " - " (:death-year author) ")")
    "")))

(defn authors->string [authors]
  (apply str (interpose ", "  (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [amount (count books)
       print-count
       (if (> amount 1)
         (str amount " books. ")
         (str amount " book. "))]
  (if (empty? books)
    "No books."
    (str print-count (apply str (interpose ". "  (map book->string books))) "." ))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))



(defn author-by-name [name authors]
  (let [hits (filter (fn [x] (= (:name x) name)) authors)]
    (if (== 0 (count hits))
      nil
      (first hits))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
  false
  true))

(defn books-by-living-authors [books]
  (filter  has-a-living-author? books))

; O______O
