(ns structured-data)

(defn do-a-thing [x] 
  (let [y (+ x x)] 
    (Math/pow y y)))

 (defn spiff [v] 
   (+ (get v 0) (get v 2)))

(defn cutify [v] 
  (conj v "<3"))

(defn spiff-destructuring [v] 
  (let [[a b c] v] (+ a c)))

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

(defn square? [rectangle] (let [[[x1 y1] [x2 y2]] rectangle] (= (- y2 y1) (- x2 x1))))

(defn area [rectangle] 
  (let [[[x1 y1] [x2 y2]] rectangle] 
    (* (- y2 y1) (- x2 x1))))

(defn contains-point? [rectangle point] 
  (let [[[x1 y1] [x2 y2]] rectangle] 
    (let [[x3 y3] point] 
      (and (<= x1 x3 x2) (<= y1 y3 y2)))))

(defn contains-rectangle? [outer inner] (let [[[x1 y1] [x2 y2]] inner] (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))))

(defn title-length [book] 
  (count (:title book)))

(defn author-count [book] 
  (count (:authors book)))

(defn multiple-authors? [book] 
  (< 1 (count (:authors book))))

(defn add-author [book new-author] 
  (assoc book :authors 
         (conj (:authors book) new-author)))

(defn alive? [author] 
  (not 
    (contains? author :death-year)))

(defn element-lengths [collection] 
  (map count collection))

(defn second-elements [collection] 
  (let [second-element (fn [c] (get c 1))] 
    (map second-element collection)))

(defn titles [books] 
  (map :title books))

(defn monotonic? [x] 
	(or (apply <= x) (apply >= x)))

(defn stars [n] 
	(apply str (repeat n "*")))

(defn toggle [a-set elem] 
(set (if (contains? a-set elem) 
	(disj a-set elem) (conj a-set elem))))

(defn contains-duplicates? [a-seq] 
(not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book] 
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author] 
  (contains? (:authors book) author))

(defn authors [book] 
  (apply clojure.set/union (map :authors book)))

(defn all-author-names [books] 
  (set (map :name (authors books))))

(defn author->string [author] (let [name (:name author) years (str " (" (:birth-year author) " - " (:death-year author) ")")] 
  (str name (if (:birth-year author) years ""))))

(defn authors->string [authors] 
  (apply str 
    (interpose ", " (map author->string authors))))

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
