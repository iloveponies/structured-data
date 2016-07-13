(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(do-a-thing 5)

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(spiff [2, 4, 3])

(defn cutify [v]
  (conj v "<3"))

(cutify [])

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(spiff-destructuring [2, 4, 3])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(width (rectangle [1 1] [5 1]))


(defn height [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(height (rectangle [1 1] [5 1]))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(square? (rectangle [3 2] [1 0]))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(area (rectangle [0 0] [4 3]))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1]))

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

(defn title-length [book]
  (count (:title book)))

(title-length cities)

(defn author-count [book]
  (count (:authors book)))

(author-count little-schemer)

(defn multiple-authors? [book]
  (> (author-count book) 1))

(multiple-authors? little-schemer)

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(add-author little-schemer {:name "Gerald J. Sussman"})

(defn alive? [author]
  (not (contains? author :death-year)))

(alive? china)

(defn element-lengths [collection]
  (map count collection))

(element-lengths ["foo" "bar" "" "quux"])

(defn second-elements [collection]
  (let [getFirst (fn [x] (get x 1))]
    (map getFirst collection)))

(get [1 2] 1)

(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])


(defn titles [books]
  (map :title books))

(def books [cities, wild-seed, embassytown, little-schemer])

(titles books)

(defn author-names [book]
  (map :name (:authors book)))

(author-names cities)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(monotonic? [1 2 1])

(defn stars [n]
  (apply str (repeat n "*")))

(stars 5)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(toggle #{:a :b :c} :d)

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(contains-duplicates? [1 5 2 3 -40])


(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(old-book->new-book
  {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
            "British Science Fiction Award"]
   :title "The City and the City"
   :authors [{:birth-year 1972, :name "China Miéville"}]})

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;
;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer"
;                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])

(defn has-author? [book author]
  (contains? (get book :authors) author))

(has-author? little-schemer octavia)

(defn authors [books]
    (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books)))))

(authors [little-schemer, cities])

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

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
