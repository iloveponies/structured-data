(ns structured-data)

Math/PI

(Math/pow 2 3)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(do-a-thing 2)

(defn spiff [v]
  (if (< (count v) 2)
    nil
    (+ (v 0) (v 1))))

(spiff [1 2 3])
(spiff [1 2 3 4 5 6])
(spiff [1 2])
(spiff [])

(defn cutify [v]
  (conj v "<3"))

(cutify [])
(cutify [1 2 3])
(cutify ["a" "b"])



(defn spiff-destructuring [v]
  v)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))))


(height (rectangle [1 1] [5 1]))
(height (rectangle [1 1] [5 5]))
(height (rectangle [0 0] [2 3]))

(width (rectangle [1 1] [5 1]))
(width (rectangle [1 1] [1 1]))
(width (rectangle [3 1] [10 4]))

(defn square? [rectangle]
  (==
    (width rectangle)
    (height rectangle)))


(square? (rectangle [1 1] [2 2]))
(square? (rectangle [1 1] [2 3]))
(square? (rectangle [1 1] [1 1]))
(square? (rectangle [3 2] [1 0]))
(square? (rectangle [3 2] [1 1]))


(defn area [rectangle]
  (*
    (width rectangle)
    (height rectangle)))


(area (rectangle [1 1] [5 1]))
(area (rectangle [0 0] [1 1]))
(area (rectangle [0 0] [4 3]))
(area (rectangle [3 1] [10 4]))

(defn x [point]
  (point 0))

(defn y [point]
  (point 1))


(defn ll [rect]
  (rect 0))

(defn ur [rect]
  (rect 1))

(defn point-less-than? [point1 point2]
  (and
    (<= (x point1) (x point2))
    (<= (y point1) (y point2))))

(point-less-than? [2 1] [2 2])

(defn contains-point? [rectangle point]
  (and
    (point-less-than? (ll rectangle) point)
    (point-less-than? point (ur rectangle))))

(contains-point? (rectangle [0 0] [2 2]) (point 2 1))


(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (ll inner))
       (contains-point? outer (ur inner))))


(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2]))
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3]))
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1]))
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2]))



(defn title-length [book]
  (count
    (:title book)))


(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name       "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title   "The Little Schemer"
                     :authors [friedman, felleisen]})

(title-length cities)                                       ;=> 21

(defn author-count [book]
  (count
    (:authors book)))


(defn multiple-authors? [book]
  (> (author-count book) 0))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))


(contains? {"a" 1} "a")                                     ;=> true
(contains? {"a" 1} 1)                                       ;=> false
(contains? {"a" nil} "a")                                   ;=> true
(contains? cities :title)                                   ;=> true
(contains? cities :name)                                    ;=> false


(defn alive? [author]
  (not
    (contains? author :death-year)))

(alive? china)                                              ;=> true
(alive? octavia)                                            ;=> false

(defn element-lengths [collection]
  (map count collection))

(element-lengths ["foo" "bar" "" "quux"])                   ;=> (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}])                  ;=> (1 3 1)


(defn second-elements [collection]
  (map second collection))


(defn titles [books]
  (map :title books))


(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name       "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title   "The Little Schemer"
                     :authors [friedman, felleisen]})

(def books [cities, wild-seed, embassytown, little-schemer])

(titles [cities])                                           ;=> ("The City and the City" )
(titles books)                                              ;=> ("The City and the City" "Wild Seed"


(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(monotonic? [1 2 3])                                        ;=> true
(monotonic? [0 1 10 11])                                    ;=> true
(monotonic? [3 2 0 -3])                                     ;=> true
(monotonic? [3 2 2])                                        ;=> true    Not strictly monotonic
(monotonic? [1 2 1 0])                                      ;=> false

(defn stars [n]
  (apply
    str (repeat n "*")))

(stars 1)                                                   ;=> "*"
(stars 7)                                                   ;=> "*******"
(stars 3)                                                   ;=> "***"

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(toggle #{:a :b :c} :d)                                     ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a)                                     ;=> #{:c :b}


(defn contains-duplicates? [a-seq]
  (not=
    (count a-seq)
    (count (set a-seq))))

(contains-duplicates? [1 1 2 3 -40])                        ;=> true
(contains-duplicates? [1 2 3 -40])                          ;=> false
(contains-duplicates? [1 2 3 "a" "a"])                      ;=> true

(defn old-book->new-book [book]
  (assoc
    book
    :authors
    (set (:authors book))))

(old-book->new-book {:title "The Little Schemer" :authors [friedman, felleisen]}) ;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
(old-book->new-book {:title "Wild Seed", :authors [octavia]}) ;=> {:title "Wild Seed", :authors #{octavia}}

(defn has-author? [book author]
  )

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
