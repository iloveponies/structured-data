(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(do-a-thing 3)

;; using maps to destructure vectors rocks!
(defn spiff [{first 0 third 2}]
  (+ first third))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[thing0 thing1 thing2]]
  (+ thing0 thing2))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

;; destructure inside a let
;; if we have brackets to the left of the thing,
;; let knows we are destructuring
(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

;; (width (rectangle [1 1] [5 1]))  ;=> 4
;; (width (rectangle [1 1] [1 1]))  ;=> 0
;; (width (rectangle [3 1] [10 4])) ;=> 7

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

;; (height (rectangle [1 1] [5 1])) ;=> 0
;; (height (rectangle [1 1] [5 5])) ;=> 4
;; (height (rectangle [0 0] [2 3])) ;=> 3

(defn square? [rectangle]
  (if (= (width rectangle)
         (height rectangle))
    true
    false))

;; (square? (rectangle [1 1] [2 2])) ;=> true
;; (square? (rectangle [1 1] [2 3])) ;=> false
;; (square? (rectangle [1 1] [1 1])) ;=> true
;; (square? (rectangle [3 2] [1 0])) ;=> true
;; (square? (rectangle [3 2] [1 1])) ;=> false

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

;; (area (rectangle [1 1] [5 1]))  => 0
;; (area (rectangle [0 0] [1 1]))  => 1
;; (area (rectangle [0 0] [4 3]))  => 12
;; (area (rectangle [3 1] [10 4])) => 21

;; goddamn destructuring is so cool
(defn contains-point? [rectangle point]
  (let [[ [x1 y1] [x2 y2] ] rectangle
        [px py] point]

    (if (and (<= x1 px x2)
             (<= y1 py y2))
      true
      false)))

;; (contains-point? (rectangle [0 0] [2 2])
;;                  (point 1 1))            ;=> true
;; (contains-point? (rectangle [0 0] [2 2])
;;                  (point 2 1))            ;=> true
;; (contains-point? (rectangle [0 0] [2 2])
;;                  (point -3 1))           ;=> false
;; (contains-point? (rectangle [0 0] [2 2])
;;                  (point 1 3))            ;=> false
;; (contains-point? (rectangle [1 1] [2 2])
;;                  (point 1 1))            ;=> true
;; (contains-point? (rectangle [1 1] [1 1])
;;                  (point 1 1))            ;=> true


(defn contains-rectangle? [outer inner]
  (let [[[in_x1 in_y1] [in_x2 in_y2]] inner
        lower_left_pt (point in_x1 in_y1)
        upper_right_pt (point in_x2 in_y2)]

    (if (and
         (contains-point? outer lower_left_pt)
         (contains-point? outer upper_right_pt))
      true
      false)))

;; (contains-rectangle? (rectangle [0 0] [3 3])
;;                      (rectangle [1 1] [2 2])) ;=> true
;; (contains-rectangle? (rectangle [0 0] [2 2])
;;                      (rectangle [1 1] [3 3])) ;=> false
;; (contains-rectangle? (rectangle [0 0] [1 1])
;;                      (rectangle [0 0] [1 1])) ;=> true
;; (contains-rectangle? (rectangle [0 0] [1 1])
;;                      (rectangle [1 1] [2 2])) ;=> false

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

(defn title-length [book]
  (count (:title book)))

;; (title-length cities)
;; (title-length wild-seed)      ;=> 9
;; (title-length little-schemer) ;=> 18

(defn author-count [book]
  (count (:authors book)))

;; (author-count cities)         ;=> 1
;; (author-count wild-seed)      ;=> 1
;; (author-count little-schemer) ;=> 2

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

;; (multiple-authors? cities)         ;=> false
;; (multiple-authors? wild-seed)      ;=> false
;; (multiple-authors? little-schemer) ;=> true

;; (assoc cities :awards ["Hugo", "World Fantasy Award",
;;                        "Arthur C. Clarke Award",
;;                        "British Science Fiction Award"])

(defn add-author [book new-author]
  (let [{name :name} new-author]
    (assoc book :authors (conj (:authors book) new-author))))



;; (add-author little-schemer {:name "Gerald J. Sussman"})
;; ;=> {:title "The Little Schemer"
;; ;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;; ;              {:name "Matthias Felleisen"}
;; ;              {:name "Gerald J. Sussman"}]}
;; (add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;; ;=> {:authors [{:name "Juhana"} {:name "Jani"}]}

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

;; (alive? china) ;=> true
;; (alive? octavia) ;=> false

(defn element-lengths [collection]
  (map count collection))

;; (element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
;; (element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)

;; dude I'm really liking destructuring
(defn second-elements [collection]
  "Takes a vector of vectors and returns
  a sequence of the second elements"
  (let [seconds #(get % 1)]
    (map seconds collection)))

(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])
;=> (2 nil "s")

(defn titles [books]
  "Takes a collection of books and
  returns their titles"
  (map :title books))

;; (def books [cities, wild-seed, embassytown, little-schemer])
;; (titles [cities]) ;=> ("The City and the City" )
;; (titles books)
;; ;=> ("The City and the City" "Wild Seed"
;; ;    "Embassytown" "The Little Schemer")

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq)
          (apply >= a-seq))
    true
    false))

;; (monotonic? [1 2 3])     ;=> true
;; (monotonic? [0 1 10 11]) ;=> true
;; (monotonic? [3 2 0 -3])  ;=> true
;; (monotonic? [3 2 2])     ;=> true    Not strictly monotonic
;; (monotonic? [1 2 1 0])   ;=> false

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [no_dupes (set a-seq)]
    (if (= (count no_dupes) (count a-seq))
      false
      true)))

;; (contains-duplicates? [1 1 2 3 -40]) ;=> true
;; (contains-duplicates? [1 2 3 -40]) ;=> false
;; (contains-duplicates? [1 2 3 "a" "a"]) ;=> true

(defn old-book->new-book [book]
  (let [ [& more] (:authors book)]
    (assoc book :authors (set more))))


;; (old-book->new-book embassytown)
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

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

;; (has-author? cities china)             ;=> true
;; (has-author? cities felleisen)         ;=> false
;; (has-author? little-schemer felleisen) ;=> true
;; (has-author? little-schemer friedman)  ;=> true
;; (has-author? little-schemer octavia)   ;=> false

(defn authors [books]
  (let [author_info
          (fn [book] (:authors book))]
    (apply clojure.set/union (map author_info books))))

;; (authors [cities, wild-seed])              ;=> #{china, octavia}
;; (authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
;; (authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}

(defn all-author-names [books]
  (set (map :name (authors books))))

;; (all-author-names books)
;; ;=> #{"Matthias Felleisen" "China Miéville"
;; ;     "Octavia E. Butler" "Daniel Friedman"}
;; (all-author-names [cities, wild-seed])
;; ;=> #{"China Miéville" "Octavia E. Butler"}
;; (all-author-names []) ;=> #{}

(defn author->string [author]
  "Takes in an author as a hashmap
  with :name, :death-year, and :birth-year keys"
  (let [{:keys [name death-year birth-year]} author]
    (if birth-year
      (str name " (" birth-year " - " death-year ")")
      (str name))))

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
;; ;   order doesn't matter

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

;; (book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
;; (book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;                                   ^-- order doesn't matter

(defn books->string [books]
  (cond
     (empty? books) "No books."
     (= 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
     :else (str (count books) " books. "
                (apply str (interpose ". " (map book->string books))) ".")))

;; (books->string []) ;=> "No books."
;; (books->string [cities])
;; ;=> "1 book. The City and the City, written by China Miéville (1972 - )."
;; (books->string [little-schemer, cities, wild-seed])
;; ;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

;; (books-by-author china books)   ;=> (cities embassytown)
;; (books-by-author octavia books) ;=> (wild-seed)


;; (def authors #{china, felleisen, octavia, friedman})

;; don't forget to use (first)
(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

;; (author-by-name "Octavia E. Butler" authors)                ;=> octavia
;; (author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
;; (author-by-name "China Miéville" authors)                   ;=> china
;; (author-by-name "Goerge R. R. Martin" authors)              ;=> nil

(defn living-authors [authors]
  "Takes in a collection of author hashmaps and returns
  all the living authors as a collection of author hashmaps"
  (filter #(alive? %) authors))

;; (living-authors authors)             ;=> (china, felleisen, friedman)
;; (living-authors #{octavia})          ;=> ()
;; (living-authors #{china, felleisen}) ;=> (china, felleisen)


;; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
;; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;; (def silmarillion {:title "Silmarillion"
;;                    :authors #{jrrtolkien, christopher, kay}})

;; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})


(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))

;; (has-a-living-author? wild-seed)      ;=> false
;; (has-a-living-author? silmarillion)   ;=> true
;; (has-a-living-author? little-schemer) ;=> true
;; (has-a-living-author? cities)         ;=> true
;; (has-a-living-author? deus-irae)      ;=> false

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

;; (books-by-living-authors books) ;=> (little-schemer cities embassytown)
;; (books-by-living-authors (concat books [deus-irae, silmarillion]))
;; ;=> (little-schemer cities embassytown silmarillion)

; %________%

















