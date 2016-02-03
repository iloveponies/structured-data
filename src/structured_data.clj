(ns structured-data)

(defn hypotenuse [x y]
    (let [xx (* x x)
          yy (* y y)]
      (Math/sqrt (+ xx yy))))

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

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
     (let [w (- x2 x1)]
        (if (< w 0) (- w) w))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (let [h (- y2 y1)]
        (if (< h 0) (- h) h))))

(defn square? [rectangle]
    (if (= (height rectangle) (width rectangle))
        true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (let [[x3 y3] point]
          (if (and
                (<= x1 x3 x2)
                (<= y1 y3 y2))
              true false))))

(defn contains-rectangle? [outer inner]
    (let [[[x1 y1] [x2 y2]] outer]
        (let [[[x3 y3] [x4 y4]] inner]
            (if (and
                    (<= x1 x3 x4 x2)
                    (<= y1 y3 y4 y2))
                true false))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [old-author-list (:authors book)]
    (assoc book :authors (conj old-author-list new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [sequence] (get sequence 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
      (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (seq a-seq) (seq (set a-seq)))
    false true))

(defn old-book->new-book [book]
  (let [old-author-list (:authors book)]
    (assoc book :authors (set old-author-list))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names (fn [book] (:authors book))]
    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (let [author-names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn authors [books]
  (let [author-names (fn [book] (:authors book))]
    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (let [author-names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [name-str (:name author)]
    (let [year-str
          (str "(" (:birth-year author) " - " (:death-year author) ")")]
     (str name-str (if (:birth-year author) (str " " year-str) "")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
    (if (= 0 (count books))
      "No books."
      (str (count books)
         (if (= (count books) 1) " book. " " books. ")
         (apply str (interpose ". " (map book->string books)))
         ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
