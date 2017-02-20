(ns structured-data)

(defn do-a-thing [x]
  (let [mo (+ x x)]
    (Math/pow mo mo)))

(defn spiff [v]
  (+(get v 0) (get v 2)))

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
   (- x2 x1)
   ))

(defn height [rectangle]
 (let [[[x1 y1] [x2 y2]] rectangle]
   (- y2 y1)
   ))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]

    (and( = (- x2 x1)(- y2 y1)))))

(defn area [rectangle]
  ( * ( width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle
          [ x3 y3] point
          ]
      (and(<= x1 x3 x2)(<= y1 y3 y2))))


(defn contains-rectangle? [outer inner]
      (let [[p1 p2] inner]
        (and(contains-point? outer p1)
            (contains-point? outer p2))))


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
  (count(:title book)))


(defn author-count [book]
  (count(:authors book)))


(defn multiple-authors? [book]
  (< 1 (author-count book)))


(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))


(defn alive? [author]
  (not (boolean (:death-year author))))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or(apply <= a-seq)
  (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n "*" )))


(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

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
