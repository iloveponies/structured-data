(ns structured-data)

(defn do-a-thing [x]
  (let [xs (+ x x)
        xa (+ x x)]
    (Math/pow xs xa)))

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
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        xx (- x2 x1)
        yy (- y2 y1)]
    (if (== xx yy) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [px py] point]
    (if (and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if (and (contains-point? outer p1) (contains-point? outer p2)) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [oldA (get book :authors)]
   (assoc book :authors (conj oldA new-author))))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [auth (get book :authors)
        s (into #{} auth)]
    (assoc book :authors s)))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (get book :authors)) books)))

(defn all-author-names [books]
  (set (map (fn [author] (get author :name)) (authors books))))

(defn author->string [author]
  (let [name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)]
    (if (contains? author :birth-year) (str name " (" birth " - " death ")") (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [nam (get book :title)
        auth (get book :authors)]
    (str nam ", written by " (apply str (authors->string auth)))))

(defn books->string [books]
  (let [c (count books)]
    (cond
     (== c 0) (str "No books.")
     (== c 1) (str "1 book. " (apply str (map book->string books)) ".")
     :else (str c " books. " (apply str (map book->string books)) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let
    [a (filter (fn [author] (= (str (get author :name)) (str name))) authors)]
    (if (empty? a) nil (first a))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [a (filter alive? (:authors book))]
    (if (empty? a) false true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
