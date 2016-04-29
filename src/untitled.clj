(use 'structured-data)

(spiff-destructuring [1 2 3])       ;=> 4
(spiff-destructuring [1 2 3 4 5 6]) ;=> 4
(spiff-destructuring [1 2])         ;=> ?
(spiff-destructuring [])            ;=> ?

(cutify []) ;=> ["<3"]
(cutify [1 2 3]) ;=> [1 2 3 "<3"]
(cutify ["a" "b"]) ;=> ["a" "b" "<3"]

(height (rectangle [1 1] [5 1])) ;=> 0
(height (rectangle [1 1] [5 5])) ;=> 4
(height (rectangle [0 0] [2 3])) ;=> 3

(width (rectangle [1 1] [5 1]))  ;=> 4
(width (rectangle [1 1] [1 1]))  ;=> 0
(width (rectangle [3 1] [10 4])) ;=> 7


(square? (rectangle [1 1] [2 2])) ;=> true
(square? (rectangle [1 1] [2 3])) ;=> false
(square? (rectangle [1 1] [1 1])) ;=> true
(square? (rectangle [3 2] [1 0])) ;=> true
(square? (rectangle [3 2] [1 1])) ;=> false

(area (rectangle [1 1] [5 1])) ; => 0
(area (rectangle [0 0] [1 1]));  => 1
(area (rectangle [0 0] [4 3]));  => 12
(area (rectangle [3 1] [10 4]));=> 21

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

(title-length cities)         ;=> 21
(title-length wild-seed)      ;=> 9
(title-length little-schemer) ;=> 18

(author-count cities)         ;=> 1
(author-count wild-seed)      ;=> 1
(author-count little-schemer) ;=> 2

(multiple-authors? cities)         ;=> false
(multiple-authors? wild-seed)      ;=> false
(multiple-authors? little-schemer) ;=> true

(add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}

(alive? china)   ;=> true
(alive? octavia) ;=> false

(element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)

(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])
;=> (2 nil "s")

(titles [cities]) ;=> ("The City and the City" )
(titles books)
;=> ("The City and the City" "Wild Seed"
;    "Embassytown" "The Little Schemer")

(stars 1) ;=> "*"
(stars 7) ;=> "*******"
(stars 3) ;=> "***"

(monotonic? [1 2 3])     ;=> true
(monotonic? [0 1 10 11]) ;=> true
(monotonic? [3 2 0 -3])  ;=> true
(monotonic? [3 2 2])     ;=> true    Not strictly monotonic
(monotonic? [1 2 1 0])   ;=> false

(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a) ;=> #{:c :b}

(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true

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

(has-author? cities china)             ;=> true
(has-author? cities felleisen)         ;=> false
(has-author? little-schemer felleisen) ;=> true
(has-author? little-schemer friedman)  ;=> true
(has-author? little-schemer octavia)   ;=> false

(authors [cities, wild-seed])              ;=> #{china, octavia}
(authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
(authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}

(all-author-names books)
;=> #{"Matthias Felleisen" "China Miéville"
;     "Octavia E. Butler" "Daniel Friedman"}
(all-author-names [cities, wild-seed])
;=> #{"China Miéville" "Octavia E. Butler"}
(all-author-names []) ;=> #{}

(author->string felleisen) ;=> "Matthias Felleisen"
(author->string friedman)  ;=> "Daniel Friedman (1944 - )"
(author->string octavia)   ;=> "Octavia E. Butler (1947 - 2006)"
(author->string {})

(authors->string (:authors little-schemer))
;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
(authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
(authors->string #{})                 ;=> ""
(authors->string #{octavia, friedman})
;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;   order doesn't matter

(book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
(book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;                                   ^-- order doesn't matter

(books->string []) ;=> "No books."
(books->string [cities])
;=> "1 book. The City and the City, written by China Miéville (1972 - )."
(books->string [little-schemer, cities, wild-seed])
;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."

(author-by-name "Octavia E. Butler" authors)                ;=> octavia
(author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
(author-by-name "China Miéville" authors)                   ;=> china
(author-by-name "Goerge R. R. Martin" authors)              ;=> nil

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

(has-a-living-author? wild-seed)      ;=> false
(has-a-living-author? silmarillion)   ;=> true
(has-a-living-author? little-schemer) ;=> true
(has-a-living-author? cities)         ;=> true
(has-a-living-author? deus-irae)      ;=> false

(books-by-living-authors books) ;=> (little-schemer cities embassytown)
(books-by-living-authors (concat books [deus-irae, silmarillion]))
;=> (little-schemer cities embassytown silmarillion)
