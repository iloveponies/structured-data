(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (let [h1 (height rectangle)
        w1 (width rectangle)]
    (== h1 w1)))

(defn area [rectangle]
  (let [h1 (height rectangle)
        w1 (width rectangle)]
    (* h1 w1)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
        (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[p1 p2] [p3 p4]] inner]
        (and
          (<= x1 p1 x2)
          (<= y1 p2 y2)
          (<= x1 p3 x2)
          (<= y1 p4 y2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (count (get book :authors))))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (let [title (fn [x] (get x :title))]
    (map title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [ecount (count a-seq)]
    (not (= ecount (count (set a-seq))))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? (set authors) author)))

(defn authors [books]
  (let [author (fn [x] (get x :authors))]
    (apply clojure.set/union (map author books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)]
          (if (contains? author :birth-year)
            (str name " (" birth " - " death ")")
            (str name))))

(defn authors->string [authors]
  (let [author (fn [x] (author->string x))]
    (apply str (interpose ", " (map author authors)))))

(defn book->string [book]
  (let [title (get book :title)
        author (authors->string (get book :authors))]
        (str title ", written by " author)))

(defn books->string [books]
  (let [book (fn [x] (book->string x))
        number (count books)]
        (if (> 1 number)
          (str "No books.")
          (if (> 2 number)
            (str number " book. " (apply str (map book books)) ".")
            (str number " books. " (apply str (interpose ". " (map book books))) ".")))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
