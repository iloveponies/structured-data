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


(height (rectangle [1 1] [5 1])) ;=> 0
(height (rectangle [1 1] [5 5])) ;=> 4
(height (rectangle [0 0] [2 3])) ;=> 3


(width (rectangle [1 1] [5 1]))  ;=> 4
(width (rectangle [1 1] [1 1]))  ;=> 0
(width (rectangle [3 1] [10 4])) ;=> 7

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(square? (rectangle [1 1] [2 2])) ;=> true
(square? (rectangle [1 1] [2 3])) ;=> false
(square? (rectangle [1 1] [1 1])) ;=> true
(square? (rectangle [3 2] [1 0])) ;=> true
(square? (rectangle [3 2] [1 1])) ;=> false

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(area (rectangle [1 1] [5 1]))  ;=> 0
(area (rectangle [0 0] [1 1]))  ;=> 1
(area (rectangle [0 0] [4 3]))  ;=> 12
(area (rectangle [3 1] [10 4])) ;=> 21



(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

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
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

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
(def books [cities, wild-seed, embassytown, little-schemer])

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
  (> (author-count book) 1))

(multiple-authors? cities)         ;=> false
(multiple-authors? wild-seed)      ;=> false
(multiple-authors? little-schemer) ;=> true

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}

(defn alive? [author]
  (not (contains? author :death-year)))
(alive? china)   ;=> true
(alive? octavia) ;=> false

(defn element-lengths [collection]
  (map (fn [x] (count x)) (seq collection)))

(element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)
(def xx (element-lengths ["x" [:a :b :c] {:y 42}]))

(defn second-elements [collection]
  (let [secndelem (fn [x] (get x 1))]
    (map secndelem collection)))

(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]]) ;=> (2 nil "s")


(defn titles [books]
  (map :title books))

(titles [cities]) ;=> ("The City and the City" )
(titles books)
;=> ("The City and the City" "Wild Seed"
;    "Embassytown" "The Little Schemer")



(defn author-names [book]
  (map :name (:authors book)))
(defn all-author-names [books]
  (set (apply concat (map author-names books))))
(all-author-names books)
;=> #{"China Miéville" "Octavia E. Butler"
;     "Daniel Friedman" "Matthias Felleisen"}
(defn all-author-names2 [books]
  (set (concat (map author-names books))))
(all-author-names2 books)

[cities,wild-seed]
(author-names little-schemer)
(map author-names [cities, wild-seed])
(concat (map author-names [cities,wild-seed]))
(apply concat (map author-names [cities,wild-seed]))



(defn stars [n]
  (apply str (repeat n \*)))
(stars 1) ;=> "*"
(stars 7) ;=> "*******"
(stars 3) ;=> "***"


(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(monotonic? [1 2 3])     ;=> true
(monotonic? [0 1 10 11]) ;=> true
(monotonic? [3 2 0 -3])  ;=> true
(monotonic? [3 2 2])     ;=> true    Not strictly monotonic
(monotonic? [1 2 1 0])   ;=> false

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a) ;=> #{:c :b}

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))
(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true


(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
  )
(old-book->new-book {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
    {:title "The Little Schemer" :authors #{friedman, felleisen}}
(old-book->new-book {:title "Wild Seed", :authors [octavia]})
;=> {:title "Wild Seed", :authors #{octavia}}
    {:title "Wild Seed", :authors #{octavia}}
(old-book->new-book
  {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
            "British Science Fiction Award"]
   :title "The City and the City"
   :authors [{:birth-year 1972, :name "China Miéville"}]})
;=> {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
;             "British Science Fiction Award"]
;    :title "The City and the City"
;    :authors #{{:birth-year 1972, :name "China Miéville"}}}


















