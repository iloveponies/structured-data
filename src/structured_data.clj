(ns structured-data)

(+ 1 1)

(defn do-a-thing [x]
  (let [plus (+ x x)]
    (Math/pow plus plus)
  )
)

(do-a-thing 1)

(defn spiff [v]
  (let [eka (get v 0)
        kolmas (get v 2)
        ]
    (+ eka kolmas)
    )
  )



(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[eka toka kolmas] v]
    (+ eka kolmas)
    )
  )

(spiff-destructuring [4 223232 4])


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[vasen a] [oikea b]] rectangle]
    (- oikea vasen)
    )
  )


(width (rectangle [1 1] [5 1]))

(width (rectangle [1 1] [1 1]))

(defn height [rectangle]
  (let [[[vasen alas] [oikea ylos]] rectangle]
    (- ylos alas)
    )
  )


(height (rectangle [1 1] [5 1]))
(height (rectangle [0 0] [2 3]))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle) )
)

(square? (rectangle [1 1] [1 1]))

(defn area [rectangle]
  (* (height rectangle) (width rectangle) )
  )

(area (rectangle [1 1] [5 1]))
(area (rectangle [0 0] [1 1]))

(area (rectangle [0 0] [4 3]))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[a b] point]
      (and (<= x1 a x2) (<= y1 b y2))
      )
    )
  )

(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))

(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left)
         (contains-point? outer inner-top-right)
         )
  )
)

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2]))

(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2]))


(defn title-length [book]
  (count (get book :title) )
  )

(defn author-count [book]
  (count (get book :authors))
  )


(defn multiple-authors? [book]
  (> (author-count book) 1)
  )


(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)
  )
)

;(add-author little-schemer {:name "Gerald J. Sussman"})
;(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true
  )
)


;(rest (seq [1 3 4 5]))



(defn element-lengths [collection]
  (map count collection)
  )

(element-lengths ["foo" "bar" "" "quux"])
(element-lengths ["x" [:a :b :c] {:y 42}])


(defn second-elements [collection]
  (let [haeToka (fn [x] (get x 1))]
    (map haeToka collection)
  )
)

(second-elements [[1 2] [2 3] [3 4]])

(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])

(defn titles [books]
  (map :title books)
)

;(titles [cities])
;(def books [cities, wild-seed, embassytown, little-schemer])
;(titles books)

(defn author-names [book]
  (map :name (:authors book)))

;(author-names little-schemer)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(monotonic? [1 2 3])
(monotonic? [1 2 1 0])
(monotonic? [3 2 0 -3])

(defn stars [n]
  (apply str (repeat n "*"))
  )

(stars 7)


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(toggle #{:a :b :c} :a)

(defn contains-duplicates? [a-seq]
  (let [joukko (set a-seq)]
    (not (= (count a-seq) (count joukko)))
    )
  )

(contains-duplicates? [1 1 2 3 -40])

(contains-duplicates? [1 2 3 -40])
(contains-duplicates? [1 2 3 "a" "a"])


(defn old-book->new-book [book]
  (let [kirjoittajat (get book :authors)]
    (assoc book :authors (set kirjoittajat))
    )
  )


(defn has-author? [book author]
  (contains? (get book :authors) author)
  )


;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})

;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer"
;                     :authors #{friedman, felleisen}})

;(def books [cities, wild-seed, embassytown, little-schemer])


(defn authors [books]
   (let [get-authors (fn [book] (get book :authors))]
     (apply clojure.set/union (map get-authors books))
    )
  )

;(authors [cities, wild-seed])


;(testi [cities, wild-seed])

(defn all-author-names [books]
  (let [get-authors-name (fn [author] (get author :name))]
    (set (map get-authors-name (authors books)))
  )
)

;(all-author-names [cities, wild-seed])

(defn author->string [author]
  (let [nimi (get author :name)]
    (if (contains? author :birth-year)
      (let [syntymavuosi (get author :birth-year)]
        (if (contains? author :death-year)
          (let [kuolemavuosi (get author :death-year)]
            (str nimi " (" syntymavuosi " - " kuolemavuosi ")")
            )
          (str nimi " (" syntymavuosi " - )")
        )
      )
      nimi
      )
    )

  )

(str "asdf" " " ")")

;(author->string friedman)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

;(authors->string #{octavia, friedman})

(defn book->string [book]
  (let [nimi (get book :title)]
    (let [kirjoittajat (get book :authors)]
        (str nimi ", written by " (authors->string kirjoittajat))
      )
    )
  )

;(book->string wild-seed)
;(book->string little-schemer)

(defn books->string [books]
  (let  [maara (count books)]
    (if (= maara 0)
      "No books."
      (if (= maara 1)
        (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
        (str (str maara " books. ") (apply str (interpose ". " (map book->string books))) ".")
        )
    )
  )
)

;(books->string [])
;(books->string [cities])
;(books->string [little-schemer, cities, wild-seed])


(defn books-by-author [author books]
  (filter (fn [kirja] (has-author? kirja author)) books
  )
)

;(books-by-author china books)


(first #{234234 4 5})

;(def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (let [etsivafunktio (fn [author] (= name (get author :name)) )]
    (first (filter etsivafunktio authors))
  )
)

;(author-by-name "China Miéville" authors)
;(author-by-name "Goerge R. R. Martin" authors)
;(author-by-name "Octavia E. Butler" #{felleisen, friedman})

(defn living-authors [authors]
  (let [etsivafunktio (fn [author] (alive? author) )]
    (filter etsivafunktio authors)
  )
)

;(living-authors authors)
;(living-authors #{octavia})
;(living-authors #{china, felleisen})


;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;(def silmarillion {:title "Silmarillion"
;                   :authors #{jrrtolkien, christopher, kay}})

;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})


(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors))))
)

;(has-a-living-author? wild-seed)
;(has-a-living-author? silmarillion)
;(has-a-living-author? deus-irae)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

;(books-by-living-authors books)

;(books-by-living-authors (concat books [deus-irae, silmarillion]))

; %________%
