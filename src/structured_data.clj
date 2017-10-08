(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

; (do-a-thing 1.00)
; [:foo 42 "bar" (+ 2 3)]

(defn spiff [v]
  (+ (get v 0) (get v 2)))

; (spiff [1 2 33])

(defn cutify [v]
  (conj v "<3"))

; (cutify [1 2])

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

; (spiff-destructuring [1 2 33])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- x1 x0)))

; (width (rectangle [1 1] [5 1]))
; (width (rectangle [1 1] [1 1]))
; (width (rectangle [3 1] [10 4]))

(defn height [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (- y1 y0)))

; (height (rectangle [1 1] [5 1]))
; (height (rectangle [1 1] [5 5]))
; (height (rectangle [0 0] [2 3]))


(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

; (square? (rectangle [1 1] [2 2])) ;=> true
; (square? (rectangle [1 1] [2 3])) ;=> false
; (square? (rectangle [1 1] [1 1])) ;=> true
; (square? (rectangle [3 2] [1 0])) ;=> true
; (square? (rectangle [3 2] [1 1])) ;=> false


(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

; (area (rectangle [1 1] [5 1]))  ;=> 0
; (area (rectangle [0 0] [1 1]))  ;=> 1
; (area (rectangle [0 0] [4 3]))  ;=> 12
; (area (rectangle [3 1] [10 4])) ;=> 21

(defn contains-point?
  "_"
  [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle
        [x y] point]
    (and (<= x0 x x1)(<= y0 y y1))))

; (contains-point? (rectangle [0 0] [2 2])
;                 (point 1 1))            ;=> true
; (contains-point? (rectangle [0 0] [2 2])
;                 (point 2 1))            ;=> true
; (contains-point? (rectangle [0 0] [2 2])
;                 (point -3 1))           ;=> false
; (contains-point? (rectangle [0 0] [2 2])
;                 (point 1 3))            ;=> false
; (contains-point? (rectangle [1 1] [2 2])
;                 (point 1 1))            ;=> true
; (contains-point? (rectangle [1 1] [1 1])
;                 (point 1 1))            ;=> true

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

; (contains-rectangle? (rectangle [0 0] [3 3])
;                     (rectangle [1 1] [2 2])) ;=> true
; (contains-rectangle? (rectangle [0 0] [2 2])
;                     (rectangle [1 1] [3 3])) ;=> false
; (contains-rectangle? (rectangle [0 0] [1 1])
;                     (rectangle [0 0] [1 1])) ;=> true
; (contains-rectangle? (rectangle [0 0] [1 1])
;                     (rectangle [1 1] [2 2])) ;=> false

; (def china {:name "China Miéville", :birth-year 1972})
; (def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
; (def friedman {:name "Daniel Friedman" :birth-year 1944})
; (def felleisen {:name "Matthias Felleisen"})

; (def cities {:title "The City and the City" :authors [china]})
; (def wild-seed {:title "Wild Seed", :authors [octavia]})
; (def embassytown {:title "Embassytown", :authors [china]})
; (def little-schemer {:title "The Little Schemer"
;                     :authors [friedman, felleisen]})


(defn title-length [book]
  (count (:title book)))

; (title-length cities)         ;=> 21
; (title-length wild-seed)      ;=> 9
; (title-length little-schemer) ;=> 18

(defn author-count [book]
  (count (:authors book)))

; (author-count cities)         ;=> 1
; (author-count wild-seed)      ;=> 1
; (author-count little-schemer) ;=> 2

(defn multiple-authors? [book]
  (< 1 (author-count book)))

; (multiple-authors? cities)         ;=> false
; (multiple-authors? wild-seed)      ;=> false
; (multiple-authors? little-schemer) ;=> true

(defn add-author [book new-author]
  (let [authors-new (conj (:authors book) new-author)
        book-new (assoc book :authors authors-new)]
    book-new))

;(add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
;(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}

(defn alive? [author]
  (not (contains? author :death-year)))

;(alive? china)   ;=> true
;(alive? octavia) ;=> false

(defn element-lengths [collection]
  (map count collection))

; (element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
; (element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)

(defn second-elements [collection]
  (let [second (fn [vector] (get vector 1))]
    (map second collection)))

; (second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
; (second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])
;=> (2 nil "s")

(defn titles [books]
  (map :title books))

; (def books [cities, wild-seed, embassytown, little-schemer])
; (titles [cities]) ;=> ("The City and the City" )
; (titles books)
;=> ("The City and the City" "Wild Seed"
;    "Embassytown" "The Little Schemer")


(defn stars [n]
  (apply str (repeat n \*)))

; (stars 1) ;=> "*"
; (stars 7) ;=> "*******"
; (stars 3) ;=> "***"

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

; (monotonic? [1 2 3])     ;=> true
; (monotonic? [0 1 10 11]) ;=> true
; (monotonic? [3 2 0 -3])  ;=> true
; (monotonic? [3 2 2])     ;=> true    Not strictly monotonic
; (monotonic? [1 2 1 0])   ;=> false

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

; (toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
; (toggle #{:a :b :c} :a) ;=> #{:c :b}

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

; (contains-duplicates? [1 1 2 3 -40]) ;=> true
; (contains-duplicates? [1 2 3 -40]) ;=> false
; (contains-duplicates? [1 2 3 "a" "a"]) ;=> true

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

; (old-book->new-book {:title "The Little Schemer"
;                     :authors [friedman, felleisen]})
;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
; (old-book->new-book {:title "Wild Seed", :authors [octavia]})
;=> {:title "Wild Seed", :authors #{octavia}}
; (old-book->new-book
;  {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
;            "British Science Fiction Award"]
;   :title "The City and the City"
;   :authors [{:birth-year 1972, :name "China Miéville"}]})


(defn has-author? [book author]
  (contains?  (:authors book) author))
; (has-author? cities china)             ;=> true
; (has-author? cities felleisen)         ;=> false
; (has-author? little-schemer felleisen) ;=> true
; (has-author? little-schemer friedman)  ;=> true
; (has-author? little-schemer octavia)   ;=> false


(defn authors [books]
  (apply clojure.set/union (map :authors books)))

; (authors [cities, wild-seed])              ;=> #{china, octavia}
; (authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
; (authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}

; (:name {:name "China Miéville", :birth-year 1972})

(defn all-author-names [books]
  (set (map :name (authors books))))

; (all-author-names books)
;=> #{"Matthias Felleisen" "China Miéville"
;     "Octavia E. Butler" "Daniel Friedman"}
; (all-author-names [cities, wild-seed])
;=> #{"China Miéville" "Octavia E. Butler"}
; (all-author-names []) ;=> #{}

(defn author->string [author]
  (let [name (str (:name author))
        years-end (if (:death-year author) (:death-year author) (str ""))
        years (if (:birth-year author) (str " (" (:birth-year author) " - " years-end ")") (str ""))
        ]
    (str name years)))

; (author->string felleisen) ;=> "Matthias Felleisen"
; (author->string friedman)  ;=> "Daniel Friedman (1944 - )"
; (author->string octavia)   ;=> "Octavia E. Butler (1947 - 2006)"

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

; (authors->string (:authors little-schemer))
;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
; (authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
; (authors->string #{})                 ;=> ""
; (authors->string #{octavia, friedman})
;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;   order doesn't matter

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

; (book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
; (book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;                                   ^-- order doesn't matter

(defn books->string [books]
  (let [book-count (count books)
        book-count-in-words (cond
                              (= 0 book-count) "No books."
                              (= 1 book-count) "1 book. "
                              (< 1 book-count) (str book-count " books. ")
                              :else "Error")
        ]
    (str book-count-in-words
         (apply str (interpose ". " (map book->string books)))
         (if (= 0 book-count) "" "."))))

; (books->string []) ;=> "No books."
; (books->string [cities])
; (books->string [little-schemer, cities, wild-seed])

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

; (books-by-author china books)   ;=> (cities embassytown)
; (books-by-author octavia books) ;=> (wild-seed)
; (def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (let [author-match (filter (fn [author] (= (:name author) name)) authors)]
    (first author-match)))

; (author-by-name "Octavia E. Butler" authors)                ;=> octavia
; (author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
; (author-by-name "China Miéville" authors)                   ;=> china
; (author-by-name "Goerge R. R. Martin" authors)              ;=> nil

(defn living-authors [authors]
  (filter alive? authors))

; (living-authors authors)             ;=> (china, felleisen, friedman)
; (living-authors #{octavia})          ;=> ()
; (living-authors #{china, felleisen}) ;=> (china, felleisen)

(defn has-a-living-author? [book]
  (let [authors (living-authors (:authors book))]
    (< 0 (count authors))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
