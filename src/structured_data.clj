(ns structured-data)

;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(defn
  do-a-thing
  [x]
  (let [x-plus-x (+ x x)]
    (Math/pow x-plus-x x-plus-x)))

(defn
  spiff
  [v]
  (if (< (count v) 3)
    \?
    (+ (get v 0) (get v 2))
  ))

(spiff [1 2 3])
(spiff [1 2 3 4 5 6])
(spiff [1 2])
(spiff [])

(defn
  cutify
  [v]
  (conj v "<3"))

(cutify [])
(cutify [1 2 3])
(cutify ["a" "b"])

(defn
  spiff-destructuring
  [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn
  point
  [x y]
  [x y])

(defn
  rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn
  abs
  [x]
  (if (< x 0)
    (* x -1)
    x))

(defn
  width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (abs (- x1 x2))))

(width (rectangle [1 1] [5 1]))
(width (rectangle [1 1] [1 1]))
(width (rectangle [3 1] [10 4]))

(defn
  height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (abs (- y1 y2))))

(height (rectangle [1 1] [5 1]))
(height (rectangle [1 1] [5 5]))
(height (rectangle [0 0] [2 3]))

(defn
  square?
  [rectangle]
    (if (== (height rectangle) (width rectangle))
      true
      false))

(square? (rectangle [1 1] [2 2]))
(square? (rectangle [1 1] [2 3]))
(square? (rectangle [1 1] [1 1]))
(square? (rectangle [3 2] [1 0]))
(square? (rectangle [3 2] [1 1]))

(defn
  area
  [rectangle]
  (* (height rectangle) (width rectangle)))

(area (rectangle [1 1] [5 1]))
(area (rectangle [0 0] [1 1]))
(area (rectangle [0 0] [4 3]))
(area (rectangle [3 1] [10 4]))

(defn
  contains-point?
  [rectangle point]
  (let [[[left_x bottom_y] [right_x top_y]] rectangle]
    (let [[point_x point_y] point]
      (if (<= left_x point_x right_x)
        (if (<= bottom_y point_y top_y)
          true
          false)
        false)
      )
  ))

(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))
(contains-point? (rectangle [0 0] [2 2])
                 (point 2 1))
(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 3))
(contains-point? (rectangle [1 1] [2 2])
                 (point 1 1))
(contains-point? (rectangle [1 1] [1 1])
                 (point 1 1))

(defn
  contains-rectangle?
  [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (if (and (contains-point? outer inner-bottom-left) (contains-point? outer inner-top-right))
             true
             false)))

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2]))
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3]))
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1]))
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2]))

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

(defn
  title-length
  [book]
  (count (:title book)))

(title-length cities)
(title-length wild-seed)
(title-length little-schemer)

(defn
  author-count
  [book]
  (count (:authors book)))

(author-count cities)
(author-count wild-seed)
(author-count little-schemer)

(defn
  multiple-authors?
  [book]
  (if (> (author-count book) 1)
    true
    false))

(multiple-authors? cities)
(multiple-authors? wild-seed)
(multiple-authors? little-schemer)

(defn
  add-author
  [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(add-author little-schemer {:name "Gerald J. Sussman"})
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})


(defn
  alive?
  [author]
  (if (contains? author :death-year)
    false
    true))

(alive? china)
(alive? octavia)

(defn
  element-lengths
  [collection]
  (map count collection))

(element-lengths ["foo" "bar" "" "quux"])
(element-lengths ["x" [:a :b :c] {:y 42}])

(defn
  second-elements
  [collection]
  (let [jotain
        (fn [vector] (get vector 1))]
    (map jotain collection
  )))

(second-elements [[1 2] [2 3] [3 4]])
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])

(defn
  titles
  [books]
  (map :title books))

(titles [cities])
(def books [cities, wild-seed, embassytown, little-schemer])
(titles books)

(defn
  monotonic?
  [a-seq]
  (if (apply <= a-seq)
    true
    (if (apply >= a-seq)
      true
      false)))

(monotonic? [1 2 3])
(monotonic? [0 1 10 11])
(monotonic? [3 2 0 -3])
(monotonic? [3 2 2])
(monotonic? [1 2 1 0])

(defn
  stars
  [n]
  (apply str (repeat n "*")))

(stars 1)
(stars 7)
(stars 3)

(defn
  toggle
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(toggle #{:a :b :c} :d)
(toggle #{:a :b :c} :a)

(defn
  contains-duplicates?
  [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
    true
    false))

(contains-duplicates? [1 1 2 3 -40])
(contains-duplicates? [1 2 3 -40])
(contains-duplicates? [1 2 3 "a" "a"])

(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(defn
  old-book->new-book
  [book]
  (assoc book :authors (set (:authors book))))

(old-book->new-book {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
(old-book->new-book {:title "Wild Seed", :authors [octavia]})
;=> {:title "Wild Seed", :authors #{octavia}}


(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])


(defn
  has-author?
  [book author]
  (if (contains? (:authors book) author )
    true
    false))

(has-author? cities china)
(has-author? cities felleisen)
(has-author? little-schemer felleisen)
(has-author? little-schemer friedman)
(has-author? little-schemer octavia)

(defn
  authors
  [books]
  (apply clojure.set/union (map :authors books)))

(authors [cities, wild-seed])
(authors [cities, wild-seed, embassytown])
(authors [little-schemer, cities])

(defn
  all-author-names
  [books]
  (set (map :name (authors books))))


(all-author-names [])
(all-author-names books)
(all-author-names [cities, wild-seed])


(defn author->string [author]
  (let [name
        (fn [author] (:name author))]
    (let [year
         (fn [author] (if (contains? author :birth-year)
                        (str " (" (:birth-year author) " - " (:death-year author) ")")))]

    (str (name author) (year author)))))



(author->string felleisen) ;=> "Matthias Felleisen"
(author->string friedman)  ;=> "Daniel Friedman (1944 - )"
(author->string octavia)   ;=> "Octavia E. Butler (1947 - 2006)"


(defn
  authors->string
  [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(authors->string (:authors little-schemer))
;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
(authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
(authors->string #{})                 ;=> ""
(authors->string #{octavia, friedman})
;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;   order doesn't matter

(defn
  book->string
  [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(book->string wild-seed)
(book->string little-schemer)


(defn
  books->string
  [books]
  (if (= (count books) 0)
    (str "No books.")
    (if (= (count books) 1)
      (apply str "1 book. " (map book->string books) ".")
      (apply str (count books) " books. " (interpose ". " (map book->string books))))
  ))

(books->string []) ;=> "No books."
(books->string [cities])
;=> "1 book. The City and the City, written by China Miéville (1972 - )."
(books->string [little-schemer, cities, wild-seed])

(defn
  books-by-author
  [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(books-by-author china books)
(books-by-author octavia books)

 (def authors #{china, felleisen, octavia, friedman})

(defn
  author-by-name
  [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
  )

(author-by-name "Octavia E. Butler" authors)                ;=> octavia
(author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
(author-by-name "China Miéville" authors)                   ;=> china
(author-by-name "Goerge R. R. Martin" authors)              ;=> nil

(defn
  living-authors
  [authors]
  (filter alive? authors))

(living-authors authors)             ;=> (china, felleisen, friedman)
(living-authors #{octavia})          ;=> ()
(living-authors #{china, felleisen}) ;=> (china, felleisen)

(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})
(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn
  has-a-living-author?
  [book]
  (not (empty? (living-authors (:authors book)))))

(has-a-living-author? wild-seed)      ;=> false
(has-a-living-author? silmarillion)   ;=> true
(has-a-living-author? little-schemer) ;=> true
(has-a-living-author? cities)         ;=> true
(has-a-living-author? deus-irae)      ;=> false

(defn
  books-by-living-authors
  [books]
  (filter has-a-living-author? books))

(books-by-living-authors books) ;=> (little-schemer cities embassytown)
(books-by-living-authors (concat books [deus-irae, silmarillion]))
;=> (little-schemer cities embassytown silmarillion)

; %________%

