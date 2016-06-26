(ns structured-data)

(defn do-a-thing [x]
  (let [two_x (+ x x)]
    (Math/pow two_x two_x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

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
  (if (= (- (height rectangle) (width rectangle)) 0) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= y1 y3 y2) (<= x1 x3 x2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
        (and (contains-point? outer point1)
             (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [author_list (:authors book)]
    (assoc book :authors (conj author_list new-author))))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)))

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
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [author_set (set (:authors book))]
    (assoc book :authors author_set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-details (fn [book] (:authors book))]
    (set (apply clojure.set/union (map author-details books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
        (cond
          (contains? author :death-year) (str name " (" birth-year " - " death-year ")")
          (contains? author :birth-year) (str name " (" birth-year " - )")
          :else (str name)
          )))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))
  ))

(defn books->string [books]
  (case (count books)
     0 (str "No books.")
     1 (str "1 book. " (book->string (first books)) ".")
     (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [matches (filter (fn [x] (= name (:name x))) authors)]
    (if (= 0 (count matches))
      nil
      (first matches) )))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
