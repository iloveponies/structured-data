(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (if (< (count v) 3)
    nil
    (let [first (get v 0)
          third (get v 2)]
      (+ first third))))
  
(spiff [1 2 3])       ;=> 4
(spiff [1 2 3 4 5 6]) ;=> 4
(spiff [1 2])         ;=> ?
(spiff [])            ;=> ?

(defn cutify [v]
  (conj v "<3"))

(cutify [])  ; => ["<3"]
(cutify [1 2 3]) ;=> [1 2 3 "<3"]
(cutify ["a" "b"]) ;=> ["a" "b" "<3"]

(defn spiff-destructuring [v]
  (if (< (count v) 3)
    nil
  (let [[first _ third] v]
    (+ first third))))

(spiff-destructuring [1 2 3])       ;=> 4
(spiff-destructuring [1 2 3 4 5 6]) ;=> 4
(spiff-destructuring [1 2])         ;=> ?
(spiff-destructuring [])            ;=> ?


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))
  
(width (rectangle [1 1] [5 1]))  ;=> 4
(width (rectangle [1 1] [1 1]))  ;=> 0
(width (rectangle [3 1] [10 4])) ;=> 7

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(height (rectangle [1 1] [5 1])) ;=> 0
(height (rectangle [1 1] [5 5])) ;=> 4
(height (rectangle [0 0] [2 3])) ;=> 3

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

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
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

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

(defn title-length [book]
  (count (:title book)))

;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors [china]})
;; (def wild-seed {:title "Wild Seed", :authors [octavia]})
;; (def embassytown {:title "Embassytown", :authors [china]})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})

;; (title-length cities)         ;=> 21
;; (title-length wild-seed)      ;=> 9
;; (title-length little-schemer) ;=> 18

(defn author-count [book]
  (count (:authors book)))

;; (author-count cities)         ;=> 1
;; (author-count wild-seed)      ;=> 1
;; (author-count little-schemer) ;=> 2

(defn multiple-authors? [book]
  (> (author-count book) 1))

;; (multiple-authors? cities)         ;=> false
;; (multiple-authors? wild-seed)      ;=> false
;; (multiple-authors? little-schemer) ;=> true

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))
  
;(add-author little-schemer {:name "Gerald J. Sussman"}) 
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}

; (add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}

(defn alive? [author]
  (not (contains? author :death-year)))

;; (alive? china)   ;=> true
;; (alive? octavia) ;=> false

(defn element-lengths [collection]
  (map count collection))

(element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)

(defn second-elements [collection]
  (let [take-second (fn [v] (get v 1))]
    (map take-second collection)))

;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors [china]})
;; (def wild-seed {:title "Wild Seed", :authors [octavia]})
;; (def embassytown {:title "Embassytown", :authors [china]})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})

;; (def books [cities, wild-seed, embassytown, little-schemer])

(defn titles [books]
  (map :title books))

;; (titles [cities]) ;=> ("The City and the City" )
;; (titles books)
;; ;=> ("The City and the City" "Wild Seed"
;; ;    "Embassytown" "The Little Schemer")

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

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

(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a) ;=> #{:c :b}

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true

;; TODO できない
(defn old-book->new-book [book]
  (let [title (:title book)
        authors (:authors book)]
    {:title title :authors (set authors)}))

;; (old-book->new-book {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})
;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
;; (old-book->new-book {:title "Wild Seed", :authors [octavia]})
;;=> {:title "Wild Seed", :authors #{octavia}}

(defn has-author? [book author]
  (contains? (:authors book) author))

;; (has-author? cities china)             ;=> true
;; (has-author? cities felleisen)         ;=> false
;; (has-author? little-schemer felleisen) ;=> true
;; (has-author? little-schemer friedman)  ;=> true
;; (has-author? little-schemer octavia)   ;=> false

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

;; (authors [cities, wild-seed])              ;=> #{china, octavia}
;; (authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
;; (authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}

(defn all-author-names [books]
  (let [author-names
        (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books)))))

;; (all-author-names books)
;; ;=> #{"Matthias Felleisen" "China Miéville"
;; ;     "Octavia E. Butler" "Daniel Friedman"}
;; (all-author-names [cities, wild-seed])
;; ;=> #{"China Miéville" "Octavia E. Butler"}
;; (all-author-names []) ;=> #{}

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (str name
         (when birth-year
           (str " (" birth-year " - " death-year ")")
          ))))

;; (author->string felleisen) ;=> "Matthias Felleisen"
;; (author->string friedman)  ;=> "Daniel Friedman (1944 - )"
;; (author->string octavia)   ;=> "Octavia E. Butler (1947 - 2006)"

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

;; (authors->string (:authors little-schemer))
;; ;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
;; (authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
;; (authors->string #{})                 ;=> ""
;; (authors->string #{octavia, friedman})
;; ;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

;; (book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
;; (book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"

(defn books->string [books]
  (let [count (count books)
        book-str (str (apply str (interpose ". " (map book->string books))) ".")]
    (cond
      (= count 0) "No books."
      (= count 1) (str "1 book. " book-str)
      :else (str count " books. " book-str)
      )))

;; (apply str (interpose ", " (map author->string authors))

;; (books->string []) ;=> "No books."
;; (books->string [cities])
;; ;=> "1 book. The City and the City, written by China Miéville (1972 - )."
;; (books->string [little-schemer, cities, wild-seed])
;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."


(defn books-by-author [author books]
  (let [has-author? (fn [book]
                      (contains? (:authors book) author))]
  (filter has-author? books)))

;; (books-by-author china books)   ;=> (cities embassytown)
;; (books-by-author octavia books) ;=> (wild-seed)

(defn author-by-name [name authors]
  (first (filter (fn [author]
            (= (:name author) name))
          authors)))

;; (author-by-name "Octavia E. Butler" authors)                ;=> octavia
;; (author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
;; (author-by-name "China Miéville" authors)                   ;=> china
;; (author-by-name "Goerge R. R. Martin" authors)              ;=> nil

(defn living-authors [authors]
  (filter alive? authors))

;; (living-authors authors)             ;=> (china, felleisen, friedman)
;; (living-authors #{octavia})          ;=> ()
;; (living-authors #{china, felleisen}) ;=> (china, felleisen)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

;; (def china {:name "China Miéville", :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors #{china}})
;; (def wild-seed {:title "Wild Seed", :authors #{octavia}})
;; (def embassytown {:title "Embassytown", :authors #{china}})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors #{friedman, felleisen}})

;; (def books [cities, wild-seed, embassytown, little-schemer])

;; (def authors #{china, felleisen, octavia, friedman})

; %________%
