(ns structured-data)

(defn do-a-thing [x]
  (let [two-x (+ x x)]
    (Math/pow two-x two-x)))

(defn spiff [v]
  (let [a (get v 0 0)
        b (get v 2 0)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ (if a a 0) (if c c 0))))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and (<= x1 x3 x2)
         (<= x1 x4 x2)
         (<= y1 y3 y2)
         (<= y1 y4 y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (let [num (count (:authors book))]
    (> num 1)))

(defn add-author [book new-author]
  (let [writers (:authors book)]
    (assoc book :authors (conj writers new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [coll] (get coll 1))]
    (map second-element collection)))

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
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [bk-authors (fn [book] (set (:authors book)))]
    (apply clojure.set/union (map bk-authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nm (:name author)
        bd (:birth-year author)
        dd (:death-year author)]
    (str nm
         (if bd (str " (" bd " - ") "")
         (if dd dd "")
         (if bd ")" ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [titul (:title book)
        auts (authors->string (:authors book))]
    (str titul ", written by " auts)))

(defn books->string [books]
  (let [ct (count books)]
    (cond
      (== ct 0) "No books."
      (== ct 1) (str "1 book. " (book->string (get books 0)) ".") 
      (> ct 1) (str ct " books. "
                    (apply str (interpose ". " (map book->string books)))
                    "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (if (= (:name author) name) author nil)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
