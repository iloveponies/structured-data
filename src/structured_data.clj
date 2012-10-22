(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1, y1] [x2, y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1, y1] [x2, y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1, y1] [x2, y2]] rectangle
        [p1, p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1, y1] [x2, y2]] outer
        [[x3, y3] [x4, y4]] inner]
    (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4]))))

(defn title-length [book]
  (let [t (:title book)] 
    (count t)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [new-auths (conj (:authors book) new-author)]
    (assoc book :authors new-auths)))

(defn alive? [author] (if (:death-year author) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [s (fn [x] (get x 1))]
    (map s collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq] (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (fn [x] 
                (cond 
                 (:death-year x) (str " (" (:birth-year x) " - " (:death-year x) ")")
                 (:birth-year x) (str " (" (:birth-year x) " - )")
                 :else ""))]
    (str (:name author) (years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond 
   (empty? books) "No books."
   (== 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
   :else (str 
          (count books) 
          " books. " 
          (apply str (interpose ". " (map book->string books)))
          ".")))

(defn books-by-author [author books]
  (let [apu (fn [x] (has-author? x author))]
  (filter apu books)))

(defn author-by-name [a-name authors]
  (let [goerge (fn [x] (= a-name (:name x)))
    	auths (filter goerge authors)]
    (cond
     (< 0 (count auths)) (first auths) 
     :else nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))