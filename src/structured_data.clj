(ns structured-data)



(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [[[x1 y1] [x2 y2]]]
  (== (- x2 x1) (- y2 y1)))

(defn area [r]
  (* (width r) (height r)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn contains-rectangle? [[[ox1 oy1] [ox2 oy2]] [[x1 y1] [x2 y2]]]
  (and (<= ox1 x1 ox2) (<= oy1 y1 oy2) (<= ox1 x2 ox2) (<= oy1 y2 oy2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [t (:title book) a (:authors book)]
    (if (boolean t) {:title t :authors (conj a new-author)} {:authors (conj a new-author)})))

(defn alive? [author]
  (not (boolean (:death-year author))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a]
  (or (apply <= a) (apply >= a)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
 (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let[ nam (:name author) birth (:birth-year author)
        years (if birth (str " (" birth " - " (:death-year author) ")") "")]
    (str nam years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [start (cond (== 0 (count books)) "No books."
                    (== 1 (count books)) "1 book. "
         :else (str (count books) " books. " ))]
    (if (== 0 (count books))
      start (str start (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
