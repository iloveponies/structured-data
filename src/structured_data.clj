(ns structured-data)

(defn do-a-thing [x] (let [sum (+ x x)] (Math/pow sum sum)))


(defn spiff [x] (+ (get x 2) (get x 0)))

(defn cutify [x] (conj x "<3"))


(defn spiff-destructuring [[x1 x2 x3]] (+ x1 x3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [[[x1 y1] [x2 y2]]] (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]] (- y2 y1))

(defn abs [x] (if (< x 0) (* x -1) x))

(defn square? [[rec1 rec2]] (let [[x1 y1] rec1 [x2 y2] rec2] (= (abs(- x1 x2)) (abs(- y2 y1)))))



(defn area [[rec1 rec2]] (let [[x1 y1] rec1 [x2 y2] rec2] (* (abs(- y2 y1)) (abs(- x2 x1)))))


(defn contains-point? [ [rec1 rec2] point] (let [ [x1 y1] rec1 [x2 y2] rec2 [xp yp] point] (if (and  (<= x1 xp x2) (<= y1 yp y2)) true false)))

(defn contains-rectangle? [rec1 rec2] (let [ [pair1 pair2] rec2 [x1 y1] pair1 [x2 y2] pair2] (if (and (contains-point? rec1 [x1 y1]) (contains-point? rec1 [x2 y2])) true false)))


;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
 ;             :birth-year 1947
  ;            :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})

;(def cities {:title "The City and the City" :authors [china]})
;(def wild-seed {:title "Wild Seed", :authors [octavia]})
;(def embassytown {:title "Embassytown", :authors [china]})
;(def little-schemer {:title "The Little Schemer"
 ;                    :authors [friedman, felleisen]})


(defn title-length [book] (count (get book :title)))

(defn author-count [book] (count (get book :authors)))


(defn multiple-authors? [book] (if (> (author-count book) 1)  true false))


(defn add-author [book author] (let [ authors (get book :authors)] (assoc book :authors (conj (get book :authors) author))))


(defn alive? [author] (not(contains? author :death-year)))


(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

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
