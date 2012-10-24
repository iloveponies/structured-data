(ns structured-data)

(defn do-a-thing [x]
    (let [foo (+ x x)]
        (Math/pow foo foo)
    ))
;  (Math/pow (+ x x) (+ x x)))

(defn spiff [v]
    (+ (get v 0) (get v 2))
    )

(defn cutify [v]
    (conj v "<3"))


(defn spiff-destructuring [v]
    (let [[a _ b] v] (+ a b)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [R] (let [ [[x1 _][x2 _]] R] (Math/abs (- (Math/max x1 x2) (Math/min x1 x2)))))

(defn height [R] (let [ [[_ y1][_ y2]] R] (Math/abs (- (Math/max y1 y2) (Math/min y1 y2)))))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
    (* (height rectangle) (width rectangle)))

(defn contains-point? [R P] (let [ [px py] P [[x1 y1][x2 y2]] R] (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
    (let [ [inner-topleft inner-bottomright] inner ] (and (contains-point? outer inner-topleft) (contains-point? outer inner-bottomright))
))

(defn title-length [book]
    (count (:title book)))

(defn author-count [book]
    (count (:authors book)))

(defn multiple-authors? [book]
    (> (author-count book) 1))

(defn add-author [book new-author]
    (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
    (= nil (:death-year author)))

(defn element-lengths [collection]
    (map count collection))

(defn second-elements [collection]
    (map second collection))

(defn titles [books]
    (map :title books))

(defn monotonic? [a-seq]
    (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
    (apply str (repeat n "*")))

(defn toggle [a-set elem]
    (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
    (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
    (contains? (:authors book) author))

; trolol, no clojure.set what is this why am I a gnome
;(defn authors [books]
;    (apply clojure.set/union (map :authors books)))
(defn authors [books]
    (set (apply concat (map :authors books))))

(defn all-author-names [books]
    (set (map :name (authors books))))

(defn author->string [a] (let [byear (:birth-year a) dyear (:death-year a) name (:name a)] (str name (if byear (str " (" byear " - " dyear ")") ""))))

(defn authors->string [authors]
    (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
    (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
    (if (= (count books) 0) "No books." (str (count books) " book" (if (> (count books) 1) "s. " ". ") (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
    (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [ res (filter (fn [a] (= name (:name a))) authors)]
    (if res (first res) nil)))

(defn living-authors [authors]
    (filter alive? authors))

(defn has-a-living-author? [book]
  (not (= 0 (count (filter alive? (:authors book))))))

(defn books-by-living-authors [books]
    (filter has-a-living-author? books))

; %________%
