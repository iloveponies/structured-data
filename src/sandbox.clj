(defn munge [x]
  (count x))


(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))


(element-lengths ["foo" "bar" "" "quux"])  ;=> (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}]) ;=> (1 3 1)



(defn second-elements [collection]
  (map
    (fn [coll]
      (let [[a b c] coll] b)
   ) collection)
)


(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]]) ;=> (2 nil "s")



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

(assoc cities :awards ["Hugo", "World Fantasy Award",
                       "Arthur C. Clarke Award",
                       "British Science Fiction Award"])
;=> {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
;             "British Science Fiction Award"]
;    :title "The City and the City"
;    :authors [{:birth-year 1972, :name "China Miéville"}]}


(defn title-length [book]
  (count (book :title)))



(title-length cities)         ;=> 21
(title-length wild-seed)      ;=> 9
(title-length little-schemer) ;=> 18


(defn author-count [book]
  (count(book :authors)))


(author-count cities)         ;=> 1
(author-count wild-seed)      ;=> 1
(author-count little-schemer) ;=> 2

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(multiple-authors? cities)         ;=> false
(multiple-authors? wild-seed)      ;=> false
(multiple-authors? little-schemer) ;=> true

(defn add-author [book new-author]
  (let [current (book :authors)]
    (let [new (conj current new-author)]
      (assoc book :authors new ))))

(add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}


(defn alive? [author]
  (if (nil? (author :death-year))
    true
    false
  )
)

(alive? china)   ;=> true
(alive? octavia) ;=> false

(defn titles [books]
  (map :title books))

(titles [cities]) ;=> ("The City and the City" )
(titles books)
;=> ("The City and the City" "Wild Seed"
;    "Embassytown" "The Little Schemer")

(defn stars [n]
  (apply str (repeat n "*"))
)

(stars 1) ;=> "*"
(stars 7) ;=> "*******"
(stars 3) ;=> "***"


(defn monotonic? [a-seq]
   (or (apply <= a-seq) (apply >= a-seq))
)

(monotonic? [1 2 3])     ;=> true
(monotonic? [0 1 10 11]) ;=> true
(monotonic? [3 2 0 -3])  ;=> true
(monotonic? [3 2 2])     ;=> true    Not strictly monotonic
(monotonic? [1 2 1 0])   ;=> false


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a) ;=> #{:c :b}


(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq)) true false)
)

(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true


(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors)))
)



(old-book->new-book {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
(old-book->new-book {:title "Wild Seed", :authors [octavia]})
;=> {:title "Wild Seed", :authors #{octavia}}

(old-book->new-book
  {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
            "British Science Fiction Award"]
   :title "The City and the City"
   :authors [{:birth-year 1972, :name "China Miéville"}]})
;=> {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
;             "British Science Fiction Award"]
;    :title "The City and the City"
;    :authors #{{:birth-year 1972, :name "China Miéville"}}}



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


(defn has-author? [book author]
  (if (contains? (book :authors) author) true false)
)

(has-author? cities china)             ;=> true
(has-author? cities felleisen)         ;=> false
(has-author? little-schemer felleisen) ;=> true
(has-author? little-schemer friedman)  ;=> true
(has-author? little-schemer octavia)   ;=> false

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(authors [cities, wild-seed])              ;=> #{china, octavia}
(authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
(authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(all-author-names books)
;=> #{"Matthias Felleisen" "China Miéville"
;     "Octavia E. Butler" "Daniel Friedman"}
(all-author-names [cities, wild-seed])
;=> #{"China Miéville" "Octavia E. Butler"}
(all-author-names []) ;=> #{}

(defn author->string [author]
  (str
;   (author :name)
;   (if (nil? (author :birth-year)) "" (str " (" (author :birth-year) " - "))
;   (if (nil? (author :death-year)) "" (author :death-year))
;   (if (nil? (author :birth-year)) "" ")" )
   (let [name (author :name)] name)

   (if (nil? (author :birth-year))
     ""
     (str
       (let [born (author :birth-year)] (str " (" born " - "))
       (let [died (author :death-year)] (str died ")"))
      )
   )

  ))

(author->string felleisen) ;=> "Matthias Felleisen"
(author->string friedman)  ;=> "Daniel Friedman (1944 - )"
(author->string octavia)

(defn authors->string [authors]
  (apply str(interpose ", " (map (fn [author] (author->string author)) authors)))
)



(authors->string (:authors little-schemer))
;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
(authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
(authors->string #{})                 ;=> ""
(authors->string #{octavia, friedman})
;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;   order doesn't matter

(defn book->string [book]
  (str
   (book :title) ", written by "
   (authors->string (book :authors))
  )
)

(book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
(book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;                                   ^-- order doesn't matter

(defn books->string [books]
  (if (empty? books)
      "No books."
      (str (count books) (if (> (count books) 1) " books. " " book. ")
        (apply str (interpose ", " (map (fn [book] (book->string book)) books))) ".")
  )
)


(books->string []) ;=> "No books."
(books->string [cities])
;=> "1 book. The City and the City, written by China Miéville (1972 - )."
(books->string [little-schemer, cities, wild-seed])
;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."

;(contains? (book :authors) author)

(defn books-by-author [author books]
  (filter (fn [book] (contains? (book :authors) author) ) books)
  )

(books-by-author china books)   ;=> (cities embassytown)
(books-by-author octavia books) ;=> (wild-seed)
(def authors #{china, felleisen, octavia, friedman})


(defn author-by-name [name authors]
  (first (filter (fn [author] (if (= (author :name) name) author nil)) authors))
)

(author-by-name "Octavia E. Butler" authors)                ;=> octavia
(author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
(author-by-name "China Miéville" authors)                   ;=> china
(author-by-name "Goerge R. R. Martin" authors)

(defn living-authors [authors]
  (filter (fn [author] (if (nil? (author :death-year)) author nil)) authors)
)

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





(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors))  ))
)


(has-a-living-author? wild-seed)      ;=> false
(has-a-living-author? silmarillion)   ;=> true
(has-a-living-author? little-schemer) ;=> true
(has-a-living-author? cities)         ;=> true
(has-a-living-author? deus-irae)      ;=> false



(defn books-by-living-authors [books]
  (filter (fn [book] (if (has-a-living-author? book) book nil)) books)
)


(books-by-living-authors books) ;=> (little-schemer cities embassytown)
(books-by-living-authors (concat books [deus-irae, silmarillion]))
;=> (little-schemer cities embassytown silmarillion)
























