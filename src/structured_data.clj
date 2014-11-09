(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (<= 3 (count v))
    (+ (get v 0) (get v 2))
    (str "?")))

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
    (= (Math/abs(- x1 x2)) (Math/abs (- y1 y2)))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
    (and (<= ox1 ix1) (<= oy1 iy1) (>= ox2 ix2) (>= oy2 iy2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (count (get book :authors))))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (= nil (get author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [collection] (get [collection] 1))])
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count(set a-seq)))))


(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (set (get book :authors)) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
  (and (= nil (get author :birth-year)) (= nil (get author :death-year))) (str (get author :name))
  (and (= nil (get author :death-year)) (not (= nil (get author :birth-year)))) (str (get author :name) " (" (str (get author :birth-year)) " - )")
  :else (str (get author :name) " (" (str (get author :birth-year)) " - " (str (get author :death-year)) ")")))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose ", written by " [(get book :title) (authors->string (get book :authors))])))

(defn books->string [books]
  (cond
   (< 1 (count books)) (str (apply str (count books) " books. "(interpose ". " (map book->string books))) ".")
   (= (count books) 1) (str (apply str (count books) " book. "(map book->string books)) ".")
   :else (str "No books.")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
