(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [first (get v 0) third (get v 2)]
    (+ first third)))     

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [w (- x1 x2)]
      (if (> w 0) w (- w)))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [h (- y1 y2)]
      (if (> h 0) h (- h)))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>= (author-count book) 2))

(defn add-author [book new-author]
  (let [authors (:authors book) newauthors (conj authors new-author)]
    (assoc book :authors newauthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [list] (get list 1))]
    (map seconds collection)))

(defn titles [books]
  (let [get_title (fn [book] (:title book))]
    (map get_title books)))

(defn monotonic? [a-seq]
 (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
 (contains? (:authors book) author))

(defn authors [books]
 (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author) birthyear (:birth-year author) deathyear (:death-year author)]
    (str name (if (and (nil? birthyear) (nil? deathyear)) (str "") (str " " "(" birthyear " " "-" " " deathyear ")")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [num (count books)]
    (if (= num 0) (str "No books.") 
        (str num (if (> num 1) (str " books. ") (str " book. ")) (apply str (interpose ", " (map book->string books)))"."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (nil? (first (living-authors (:authors book))))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
