(ns structured-data)

(defn do-a-thing [x]
  (let [double_x (+ x x)]
    (Math/pow double_x double_x)))

(defn cutify [v]
  (conj v "<3"))
  

(defn spiff-destructuring [[a _ b]]
  (+ a b))
;(spiff-destructuring [1 2 3])

(defn spiff [v]
  (+ (get  v 0)
     (get  v 2)))



(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(def r (rectangle [1 1] [5 5]))

(width r)
(height r)



(defn square? [rectangle]
  (= (width rectangle)
     (height rectangle)))

(def r (rectangle [1 1] [5 5]))
(def s (rectangle [0 1] [5 5]))
(square? r)
(square? s)


(defn area [rectangle]
  (* (width rectangle) 
     (height rectangle)))

(area r)
(area s)

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (>= px x1)
       (<= px x2)
       (>= py y1)
       (<= py y2)))

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



(defn contains-rectangle? [[[ox1 oy1] [ox2 oy2]]
                           [[ix1 iy1] [ix2 iy2]]]
  (and (>= ix1 ox1)
       (<= ix2 ox2)
       (>= iy1 oy1)
       (<= iy2 oy2)))

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2])) ;=> true
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3])) ;=> false
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1])) ;=> true
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2])) ;=> false


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

(title-length cities)         ;=> 21
(title-length wild-seed)      ;=> 9
(title-length little-schemer) ;=> 18

(defn author-count [book]
  (count (:authors book)))
(author-count cities)         ;=> 1
(author-count wild-seed)      ;=> 1
(author-count little-schemer) ;=> 2

(defn multiple-authors? [book]
  (>= (author-count book)
      2))

(multiple-authors? cities)         ;=> false
(multiple-authors? wild-seed)      ;=> false
(multiple-authors? little-schemer) ;=> true

(assoc cities :awards ["Hugo", "World Fantasy Award",
                       "Arthur C. Clarke Award",
                       "British Science Fiction Award"])

(defn add-author [book new-author]
  (let [current-authors (:authors book)]
    (assoc book :authors (conj current-authors new-author ))))


(add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}

(contains? {"a" 1} "a")   ;=> true
(contains? {"a" 1} 1)     ;=> false
(contains? {"a" nil} "a") ;=> true
(contains? cities :title) ;=> true
(contains? cities :name)  ;=> false

(defn alive? [author]
  (not (contains? author :death-year)))

(alive? china)   ;=> true
(alive? octavia) ;=> false

(defn element-lengths [collection]
  (map count collection))

(element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)


(defn second-elements [collection]
  (map second collection))

(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])
;=> (2 nil "s")


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


(defn titles [books]
  (map :title books))

(titles [cities]) ;=> ("The City and the City" )
(titles books)
;=> ("The City and the City" "Wild Seed"
;    "Embassytown" "The Little Schemer")

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(monotonic? [1 2 3])     ;=> true
(monotonic? [0 1 10 11]) ;=> true
(monotonic? [3 2 0 -3])  ;=> true
(monotonic? [3 2 2])     ;=> true    Not strictly monotonic
(monotonic? [1 2 1 0])   ;=> false

(defn stars [n]
  (apply str (repeat n "*")))

(stars 1) ;=> "*"
(stars 7) ;=> "*******"
(stars 3) ;=> "***"

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

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


((foo bar () baz quux))
