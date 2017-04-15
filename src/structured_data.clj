(ns structured-data)

(defn do-a-thing [x]
  (let [thing (+ x x)]
     (Math/pow thing thing)))

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
     (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
     (if (== (- x1 x2) (- y1 y2)) true false)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[x1 y1][x2 y2]] rectangle]
    (boolean(and (<= x1 px x2) (<= y1 py y2)))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (boolean (and (contains-point? outer point1) (contains-point? outer point2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (boolean (> (author-count book) 1)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (boolean (not (contains? author :death-year))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (boolean (or (apply <= a-seq) (apply >= a-seq))))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (boolean (not (== (count a-seq) (count (set a-seq))))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (boolean (contains? (:authors book) author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  )

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  (filter (fn [x](has-author? x author)) books))

(defn author-by-name [name authors]
  (first(filter (fn [x](= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        living (living-authors authors)
        count-living (count living)]
    (if (> count-living 0)
      true
      false)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%



