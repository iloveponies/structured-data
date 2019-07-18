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
  (contains? (:authors book) author))
(has-author? cities china)                   ;=> true
(has-author? cities felleisen)               ;=> false
(has-author? little-schemer felleisen)       ;=> true
(has-author? little-schemer friedman)        ;=> true
(has-author? little-schemer octavia)         ;=> false

(defn authors [books]
  (apply clojure.set/union (map :authors books)))
(authors [cities, wild-seed])              ;=> #{china, octavia}
(authors [cities, wild-seed, embassytown])
;=> #{china, octavia}
(def vertailu #{china, octavia})


(authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}


(defn all-author-names [books]
  (set (map :name (authors books))))
(all-author-names books)
;=> #{"Matthias Felleisen" "China Miéville"
;     "Octavia E. Butler" "Daniel Friedman"}
(all-author-names [cities, wild-seed])
;=> #{"China Miéville" "Octavia E. Butler"}
(all-author-names []) ;=> #{}


(defn zauthor->string [author]
  (let [name (:name author)
        years (cond
               (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
               (contains? author :birth-year) (str " (" (:birth-year author) " - )")
               )]
    (str name years)))

(defn xauthor->string [author]
  (let [name (:name author)
        years (if (contains? author :birth-year) (str " (" (:birth-year author) " - " (:death-year author) ")"))
       ]
    (str name years)))

(defn author->string [author]
  (let [name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author) ")")
       ]
    (str name (if (contains? author :birth-year) years))))

(author->string felleisen) ;=> "Matthias Felleisen"
(author->string friedman)  ;=> "Daniel Friedman (1944 - )"
(author->string octavia)   ;=> "Octavia E. Butler (1947 - 2006)"

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(authors->string (:authors little-schemer))
;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
(authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
(authors->string #{})                 ;=> ""
(authors->string #{octavia, friedman})
;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;   order doesn't matter


(defn book->string [book]
  (if (> (count book) 0) (str (:title book) ", written by "(authors->string (:authors book)))))
(book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
(book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;                                   ^-- order doesn't matter

(defn books->string [books]
  (let [
        bookcount (cond
                   (= (count books) 0) "No books."
                   (= (count books) 1) "1 book. "
                   :else (str (count books) " books. ")
                   )
        booklist (apply str (interpose ". " (map book->string books)))]
    (str bookcount booklist)))
(books->string []) ;=> "No books."
(books->string [cities])
;=> "1 book. The City and the City, written by China Miéville (1972 - )."
(books->string [little-schemer, cities, wild-seed])
;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))


(books-by-author china books)   ;=> (cities embassytown)
(books-by-author octavia books) ;=> (wild-seed)

(def authors #{china, felleisen, octavia, friedman})
(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name )) authors)))

(author-by-name "Octavia E. Butler" authors)                ;=> octavia
(author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
(author-by-name "China Miéville" authors)                   ;=> china
(author-by-name "Goerge R. R. Martin" authors)              ;=> nil

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))
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
  (seq (living-authors (:authors book))))

(has-a-living-author? wild-seed)      ;=> false
(has-a-living-author? silmarillion)   ;=> true
(has-a-living-author? little-schemer) ;=> true
(has-a-living-author? cities)         ;=> true
(has-a-living-author? deus-irae)      ;=> false

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

(books-by-living-authors books) ;=> (little-schemer cities embassytown)
(books-by-living-authors (concat books [deus-irae, silmarillion]))
;=> (little-schemer cities embassytown silmarillion)



