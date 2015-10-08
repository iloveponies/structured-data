(ns structured-data)

(defn hypotenuse [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)))

(defn sum-pairs [first-pair second-pair]
  [(+ (first first-pair) (first second-pair))
   (+ (second first-pair) (second second-pair))])

(defn sum-pairs-pattern [[xone yone] [xtwo ytwo]]
  [(+ xone xtwo) (+ yone ytwo)])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[xone _] [xtwo _]] rectangle]
    (- xtwo xone)))

(defn height [rectangle]
  (let [[[_ yone] [_ ytwo]] rectangle]
    (- ytwo yone)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[xone yone] [xtwo ytwo]] rectangle
        [x y] point]
    (and (<= xone x xtwo) (<= yone y ytwo))))

(defn contains-rectangle? [outer inner]
  (let [[plower pupper] inner]
    (and (contains-point? outer plower) (contains-point? outer pupper))))


(def book {:title "The city and the city"
           :authors [{:name "China Miéville" :birth-year 1972}]})

(def china {:name "China Miéville"  :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})
(def cities {:title "The city and the city" :authors [china]})
(def wild-seed {:title "Wild Seed" :authors [octavia]})
(def embassytown {:title "Embassytown" :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman felleisen]})



(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>= (count (:authors book)) 2))

(defn add-author [book new-author]
  (let [final-authors (conj (:authors book) new-author)]
    (assoc book :authors final-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

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
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

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
