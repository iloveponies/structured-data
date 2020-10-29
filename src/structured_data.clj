(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(spiff [1 2 3])       ;=> 4
(spiff [1 2 3 4 5 6]) ;=> 4
;; (spiff [1 2])         ;=> ?
;; (spiff [])            ;=> ?

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(spiff-destructuring [1 2 3])       ;=> 4
(spiff-destructuring [1 2 3 4 5 6]) ;=> 4

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(width (rectangle [1 1] [5 1]))
(width (rectangle [1 1] [1 1]))
(width (rectangle [3 1] [10 4]))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(height (rectangle [1 1] [5 1]))
(height (rectangle [1 1] [5 5]))
(height (rectangle [0 0] [2 3]))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(square? (rectangle [1 1] [2 2])) ;=> true
(square? (rectangle [1 1] [2 3])) ;=> false
(square? (rectangle [1 1] [1 1])) ;=> true
(square? (rectangle [3 2] [1 0])) ;=> true
(square? (rectangle [3 2] [1 1])) ;=> false

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(area (rectangle [1 1] [5 1]))  ;> 0
(area (rectangle [0 0] [1 1]))  ;> 1
(area (rectangle [0 0] [4 3]))  ;> 12
(area (rectangle [3 1] [10 4])) ;> 21

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
    (and (<= x1 a x2) (<= y1 b y2))))

(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point 2 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))           ;=> false
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 3))            ;=> false
(contains-point? (rectangle [1 1] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [1 1] [1 1])
                 (point 1 1))            ;=> true

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and
     (<= x1 x3 x2)
     (<= x1 x4 x2)
     (<= y1 y3 y2)
     (<= y1 y4 y2))))

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2])) ;=> true
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3])) ;=> false
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1])) ;=> true
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2])) ;=> false

(defn title-length [book]
  (count (:title book)))

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(def books [cities, wild-seed, embassytown, little-schemer])

(title-length cities)         ;=> 21
(title-length wild-seed)      ;=> 9
(title-length little-schemer) ;=> 18

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original (:authors book)
        new (conj original new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (if (and (contains? author :death-year) (< (:death-year author) 2020))
    false
    true))

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
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (set (apply concat (map author-names books))))

(defn all-author-names [books]
  (let [author-names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (:death-year author)
                (str " (" (:birth-year author) " - " (:death-year author) ")") "")]
    (str name years)))


(defn authors->string [authors]
  (interpose ", " (map author->string authors)))

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
