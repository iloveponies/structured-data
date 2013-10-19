(ns structured-data)

(defn do-a-thing [x]
  (let [xdouble (+ x x)]
    (Math/pow xdouble xdouble)))

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
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right]
    (- x2 x1)))

(defn height [rectangle]
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[bottom-left top-right] rectangle
        [x1 y1] bottom-left
        [x2 y2] top-right
        [x, y]  point]
    (and
      (>= x x1)
      (<= x x2)
      (>= y y1)
      (<= y y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and
      (contains-point? outer inner-bottom-left)
      (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (get book :authors) new-author )))

(defn alive? [author]
  (if (get author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count (seq collection)))

(defn second-elements [collection]
  (map second (seq collection)))

(defn titles [books]
  (map (fn [book] (get book :title)) (seq books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq))
    (count a-seq))))

(defn old-book->new-book [book]
  (let [authors (fn [b] (get b :authors))]
    (assoc book :authors (set (authors book)))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth (get author :birth-year)
        death (get author :death-year)
        name  (get author :name)]
    (str name
      (if birth 
        (str " (" birth " - "
          (if death
            (str death ")")
            ")"))
        ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
  (let [infos (apply str (interpose ", " (map book->string books)))
        num   (count books)]
    (str
      (cond
        (== num 0) "No books"
        (== num 1) "1 book. "
        :else (str num " books. "))
      infos ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first
    (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
