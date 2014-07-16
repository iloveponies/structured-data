(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

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

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [xp yp]]
  (if (and(<= x1 xp x2)
          (<= y1 yp y2))
    true
    false))

(defn contains-rectangle? [outer inner]
  (let [[inner1 inner2] inner]
    (if (and (contains-point? outer inner1)
             (contains-point? outer inner2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-elm (fn [collection] (get collection 1))]
    (map second-elm collection)))

(defn second-elements2 [collection]
  (let [second-elm (fn [[a b]] b)]
    (map second-elm collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

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
(comment (def china {:name "China Miéville", :birth-year 1972})
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

         (old-book->new-book {:title "The Little Schemer"
                              :authors [friedman, felleisen]}))
