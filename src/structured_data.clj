(ns structured-data)

;; Exercise 1
;; The following function does a thing:
;; (defn do-a-thing [x]
;;   (Math/pow (+ x x) (+ x x)))
;; Change the function do-a-thing so that it uses let to give a name to the common expression (+ x x) in its body.
;; (defn do-a-thing [x]
;;   (Math/pow (+ x x) (+ x x)))
(defn do-a-thing [x]
  (let [x-times-2 (+ x x)]
    (Math/pow x-times-2 x-times-2)))


;; Exercise 2
;; Write the function (spiff v) that takes a vector and returns the sum of the first and third elements of the vector. What happens when you pass in a vector that is too short?
;; (spiff [1 2 3])       ;=> 4
;; (spiff [1 2 3 4 5 6]) ;=> 4
;; (spiff [1 2])         ;=> ?
;; (spiff [])            ;=> ?
(defn spiff [v]
  (+ (get v 0) (get v 2)))

;; Exercise 3
;; Write the function (cutify v) that takes a vector as a parameter and adds "<3" to its end.
;; (cutify []) => ["<3"]
;; (cutify [1 2 3]) => [1 2 3 "<3"]
;; (cutify ["a" "b"]) => ["a" "b" "<3"]
;;
;; vector -> vector
;; Add <3 at the end
(defn cutify [v]
  (conj v "<3"))

;; Exercise 4
;; Rewrite our earlier function spiff by destructuring its parameter. Call this new function spiff-destructuring.
(defn spiff-destructuring [v]
  (let [[a _ c] v]
    (+ a c)))
;; Simpler
(defn spiff-destructuring [[a _ c]]
  (+ a c))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

;; Exercise 5
;; Write the functions (height rectangle) and (width rectangle) that return the height and width of the given rectangle. Use destructuring.
(defn width [[[x1 y1] [x2 y2]]]
  (let [res (- x1 x2)]
    (if (>= res 0 )
      res
      (- res))))

(defn height [[[x1 y1] [x2 y2]]]
  (let [res (- y1 y2)]
    (if (>= res 0 )
      res
      (- res))))

;; (height (rectangle [1 1] [5 1])) ;=> 0
;; (height (rectangle [1 1] [5 5])) ;=> 4
;; (height (rectangle [0 0] [2 3])) ;=> 3

;; (width (rectangle [1 1] [5 1]))  ;=> 4
;; (width (rectangle [1 1] [1 1]))  ;=> 0
;; (width (rectangle [3 1] [10 4])) ;=> 7


;; Exercise 6
;; Write the function (square? rectangle) that returns true if rectangle is a square and otherwise false.
;; (square? (rectangle [1 1] [2 2])) ;=> true
;; (square? (rectangle [1 1] [2 3])) ;=> false
;; (square? (rectangle [1 1] [1 1])) ;=> true
;; (square? (rectangle [3 2] [1 0])) ;=> true
;; (square? (rectangle [3 2] [1 1])) ;=> false
(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

;; Exercise 7
;; Write the function (area rectangle) that returns the area of the given rectangle.
(defn area [rectangle]
  (* (height rectangle) (width rectangle)))
;; (area (rectangle [1 1] [5 1]))  ;=> 0
;; (area (rectangle [0 0] [1 1]))  ;=> 1
;; (area (rectangle [0 0] [4 3]))  ;=> 12
;; (area (rectangle [3 1] [10 4])) ;=> 21


;; Exercise 8
;; Write the function (contains-point? rectangle point) that returns true if rectangle contains point and otherwise false.
;; Remember that you can give <= multiple parameters. (<= x y z) returns true if x≤y≤z holds. Otherwise false.
;; Hint: and is useful.
;; use destructuring.
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    ;; Generalized version that can handle x2 < x1, etc
    (if (and (or (<= x1 x x2) (>= x1 x x2))
             (or (<= y1 y y2) (>= y1 y y2)))
      true
      false)))
;;
;; no need for if when using predicates
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    ;; Generalized version that can handle x2 < x1, etc
    (and (or (<= x1 x x2) (>= x1 x x2))
         (or (<= y1 y y2) (>= y1 y y2)))))
;;
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


;; Exercise 9
;; Write the function (contains-rectangle? outer inner) that returns true if the rectangle inner is inside the rectangle outer and otherwise false.
;; Hint: use contains-point?
;;
;; Two nested vectors -> Bool
;; Check if all four points are within the first rectangle
;; Get all four points from inner
(defn contains-rectangle? [outer [[x1 y1] [x2 y2]]]
  ;; Check all four points with AND condition
  (and (contains-point? outer [x1 y1])
       (contains-point? outer [x1 y2])
       (contains-point? outer [x2 y1])
       (contains-point? outer [x2 y2])))
;;
;; for solution
(defn contains-rectangle? [outer [[x1 y1] [x2 y2]]]
  ;; Check all four points with for
  (let [bool-4-points (for [x [x1 x2] y [y1 y2]]
                        (contains-point? outer [x y]))]
    ;; true if true X 4
    (every? identity bool-4-points)))
;;
(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2])) ;=> true
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3])) ;=> false
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1])) ;=> true
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2])) ;=> false


;; Exercise 10
;; Write the function (title-length book) that counts the length of the book’s title.
;; (title-length cities)         ;=> 21
;; (title-length wild-seed)      ;=> 9
;; (title-length little-schemer) ;=> 18
;; Map -> Number
;; Length of :title element of the map
(defn title-length [book]
  (count (:title book)))

;; Exercise 11
;; Write the function (author-count book) that returns the amount of authors that book has.
;; (author-count cities)         ;=> 1
;; (author-count wild-seed)      ;=> 1
;; (author-count little-schemer) ;=> 2
(defn author-count [book]
  (count (:authors book)))

;; Exercise 12
;; Write the function (multiple-authors? book) that returns true if book has multiple authors, otherwise false.
;; (multiple-authors? cities)         ;=> false
;; (multiple-authors? wild-seed)      ;=> false
;; (multiple-authors? little-schemer) ;=> true
(defn multiple-authors? [book]
  (>= (author-count book) 2))


;; Exercise 13
;; Use assoc and conj to write the function (add-author book new-author) that takes a book and an author as a parameter and adds author to books authors.
;; Hint: use let to avoid pain
;;
;; map -> map
;; Add author by extracting author part, add author, and conj
(defn add-author [book new-author]
  ;; Extract authors part
  (let [authors (:authors book)]
    ;; Add a new authors to authors part, then add to map (overwrite)
    (assoc book :authors (conj authors new-author))))
;; (add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}


;; Exercise 14
;; Write the function (alive? author) which takes an author map and returns true if the author is alive, otherwise false.
;; An author is alive if the author does not have a death year.
;; (alive? china)   ;=> true
;; (alive? octavia) ;=> false
;;
;; map -> bool
;; Check for presence of :death-year element in the map
(defn alive? [author]
  (not (contains? author :death-year)))


;; Exercise 15
;; Write the function (element-lengths collection) that returns the lengths of every item in collection.
;; (element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
;; (element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)
(defn element-lengths [collection]
  (map count collection))

;; Exercise 16
;; Use map to write the function (second-elements collection) that takes a vector of vectors and returns a sequence of the second elements.
;; Remember that you can use get to index a vector.
;; Use fn and let to create a helper function and use it with map.
;; (second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
;; (second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])
;; ;=> (2 nil "s")
(defn second-elements [collection]
  (map second collection))

;; Exercise 17
;; Write the function (titles books) that takes a collection of books and returns their titles.
(defn titles [books]
  (map :title books))
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
;; ;; titles should work like this:
;; (titles [cities]) ;=> ("The City and the City" )
;; (titles books)
;; ;=> ("The City and the City" "Wild Seed"
;; ;    "Embassytown" "The Little Schemer")


;; Exercise 19
;; Write the function (monotonic? a-seq) that returns true if a-seq is monotonic and otherwise false.
;; A sequence is monotonic if is either inceasing or decreasing. In a decreasing sequence every element is at most as large as the previous one and in an increasing sequence every member is at least as large as the previous one.
;; Use apply.
;; Hint: <= might be useful
;; (monotonic? [1 2 3])     ;=> true
;; (monotonic? [0 1 10 11]) ;=> true
;; (monotonic? [3 2 0 -3])  ;=> true
;; (monotonic? [3 2 2])     ;=> true    Not strictly monotonic
;; (monotonic? [1 2 1 0])   ;=> false
(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

;; Exercise 18
;; Write the function (stars n) that returns a string with n aterisks \*.
;; The function (repeat n x) returns a sequence with n xs:
;; (repeat 5 "*") ;=> ("*" "*" "*" "*" "*")
;; (repeat 3 "~o~") ;=> ("~o~" "~o~" "~o~")
;; Remember that you can use str to concatenate strings.
;; (stars 1) ;=> "*"
;; (stars 7) ;=> "*******"
;; (stars 3) ;=> "***"
(defn stars [n]
  (apply str (repeat n "*")))


;; Exercise 20
;; Write the function (toggle a-set elem) that removes elem from a-set if a-set contains elem, and adds it to the set otherwise.
;; (toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
;; (toggle #{:a :b :c} :a) ;=> #{:c :b}
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


;; Exercise 21
;; Write the function (contains-duplicates? sequence) that takes a sequence as a parameter and returns true if sequence contains some element multiple times. Otherwise it returns false.
;; (contains-duplicates? [1 1 2 3 -40]) ;=> true
;; (contains-duplicates? [1 2 3 -40]) ;=> false
;; (contains-duplicates? [1 2 3 "a" "a"]) ;=> true
(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

;; Exercise 22
;; Write the function (old-book->new-book book) that takes a book with the previous representation (authors in a vector) and returns the same book in the new representation (authors in a set).
;; Use assoc to change the representation. Do not construct a new map using the map literal syntax.
;; (old-book->new-book {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})
;; ;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
;; (old-book->new-book {:title "Wild Seed", :authors [octavia]})
;; ;=> {:title "Wild Seed", :authors #{octavia}}
;; The reason to use assoc is that it allows us to keep any additional key-value pairs intact. Earlier we had an example where we added a list of awards to a book. By using assoc, these additional key-value pairs do not disappear anywhere during the transformation.
;; (old-book->new-book
;;   {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
;;             "British Science Fiction Award"]
;;    :title "The City and the City"
;;    :authors [{:birth-year 1972, :name "China Miéville"}]})
;; ;=> {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
;; ;             "British Science Fiction Award"]
;; ;    :title "The City and the City"
;; ;    :authors #{{:birth-year 1972, :name "China Miéville"}}}
;;
;; map -> map
;; Change author part from a vector to a set
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

;; Exercise 23
;; Write the function (has-author? book author) that returns true if author is in the authors of book and otherwise false.
;; (has-author? cities china)             ;=> true
;; (has-author? cities felleisen)         ;=> false
;; (has-author? little-schemer felleisen) ;=> true
;; (has-author? little-schemer friedman)  ;=> true
;; (has-author? little-schemer octavia)   ;=> false
;;
;; map -> bool
(defn has-author? [book author]
  (contains? (:authors book) author))


;; Exercise 24
;; Write the function (authors books) that returns the authors of every book in books as a set.
;; (authors [cities, wild-seed])              ;=> #{china, octavia}
;; (authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
;; (authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}
;;
;; map -> set
;; extract authors and return as a set of these using union
;; Utilize has-author
(defn authors [books]
  (apply clojure.set/union (map (fn [book] (:authors book)) books)))



;; Exercise 25
;; Write the function (all-author-names books) that works like the previous one and uses authors.
;; (all-author-names books)
;; ;=> #{"Matthias Felleisen" "China Miéville"
;; ;     "Octavia E. Butler" "Daniel Friedman"}
;; (all-author-names [cities, wild-seed])
;; ;=> #{"China Miéville" "Octavia E. Butler"}
;; (all-author-names []) ;=> #{}
(defn all-author-names [books]
  (set (map :name (authors books))))


;; Exercise 26
;; Write the function (author->string author) that returns a string representation of author as follows:
;; You can assume that every author with a :death-year also has a :birth-year.
;; (author->string felleisen) ;=> "Matthias Felleisen"
;; (author->string friedman)  ;=> "Daniel Friedman (1944 - )"
;; (author->string octavia)   ;=> "Octavia E. Butler (1947 - 2006)"
;; Hint: you probably want to split this string into two parts: name and years. Use let to form these and use str to create the final string.
;; map -> string
;; if present, format 
(defn author->string [author]
  (str (:name author)
       ;; Return formatted years if :birth-year is contained.
       (if (contains? author :birth-year)
         (str " (" (:birth-year author) " - " (:death-year author) ")"))))


;; Exercise 27
;; Write the function (authors->string authors) which takes a sequence of authors as a parameter and returns a string representation of authors in the following manner:
;; (authors->string (:authors little-schemer))
;; ;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
;; (authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
;; (authors->string #{})                 ;=> ""
;; (authors->string #{octavia, friedman})
;; ;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;; ;   order doesn't matter
;; Since the authors are in a set, which doesn’t have a predefined order, the resulting string can have the authors in any order.
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


;; Exercise 28
;; Write the function (book->string book) takes a single book as a parameter and returns a string representation of book as follows:
;; (book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
;; (book->string little-schemer)
;; ;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;; ;                                   ^-- order doesn't matter
;; Again, the order of authors in the string doesn’t matter.
(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

;; Exercise 29
;; Write the function (books->string books) that takes a sequence of books as a parameter and returns a string representation of books like this:
;; (books->string []) ;=> "No books."
;; (books->string [cities])
;; ;=> "1 book. The City and the City, written by China Miéville (1972 - )."
;; (books->string [little-schemer, cities, wild-seed])
;; ;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."
;;
;; map -> string
;; 3 patterns depending on the number of books. State the number of books. Then use book->string for each book, add 
(defn books->string [books]
  (let [n-books (count books)]
    (cond
     (= n-books 0) "No books."
     (= n-books 1) (str "1 book. " (apply book->string books) ".")
     (> n-books 1) (str n-books " books. "
                        ;; combine together
                        (apply str
                               ;; Add a single space in between
                               (interpose " "
                                          ;; Add . at the end of each string.
                                          (map #(str % ".")
                                               ;; Use book->string for each book
                                               (map book->string books))))))))

;; Exercise 30
;; Write the function (books-by-author author books).
;; Hint: has-author?
;; (books-by-author china books)   ;=> (cities embassytown)
;; (books-by-author octavia books) ;=> (wild-seed)
;;
;; map -> list
;; Return books having author as one of the authors
(defn books-by-author [author books]
  (filter #(has-author? % author)  books))

;; Exercise 31
;; Write the function (author-by-name name authors) that takes a string name and a sequence of authors and returns an author with the given name if one is found. If one is not found, then nil should be returned.
;; Hint: remember first
;; (author-by-name "Octavia E. Butler" authors)                ;=> octavia
;; (author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
;; (author-by-name "China Miéville" authors)                   ;=> china
;; (author-by-name "Goerge R. R. Martin" authors)              ;=> nil
;;
;; 
(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))


;; Exercise 32
;; Write the function (living-authors authors) that takes a sequence of authors and returns those that are alive. Remember alive?.
;; (living-authors authors)             ;=> (china, felleisen, friedman)
;; (living-authors #{octavia})          ;=> ()
;; (living-authors #{china, felleisen}) ;=> (china, felleisen)
;; The order in the results doesn’t matter.
;;
;; map -> seq
;; Keep if the author does not have :death-year
(defn living-authors [authors]
  (filter #(not (contains? % :death-year)) authors))

;; Exercise 33
;; Write the function (has-a-living-author? book) that returns true if book has a living author, and otherwise false.
;; (has-a-living-author? wild-seed)      ;=> false
;; (has-a-living-author? silmarillion)   ;=> true
;; (has-a-living-author? little-schemer) ;=> true
;; (has-a-living-author? cities)         ;=> true
;; (has-a-living-author? deus-irae)      ;=> false
(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


;; Exercise 34
;; Write the function (books-by-living-authors books) that takes a sequence of books as a parameter and returns those that have a living author.
;; (books-by-living-authors books) ;=> (little-schemer cities embassytown)
;; (books-by-living-authors (concat books [deus-irae, silmarillion]))
;; ;=> (little-schemer cities embassytown silmarillion)
(defn books-by-living-authors [books]
  (filter has-a-living-author?  books))

; %________%
