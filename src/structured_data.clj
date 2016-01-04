(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
  )

(do-a-thing 2)

(defn spiff [v]

  (+ (get v 0) (get v 2))

  )

(assoc [1 2 3 4] 2 "foo")

(defn cutify [v]
  (conj v "<3")

  )

(cutify [1 2 3])

(defn spiff-destructuring [v]
  (let [[x y z]  v]
    (+ x z))
  )

(spiff-destructuring [1 2 3])

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
          (- x2 x1)
       )
  )

(width (rectangle [1 1] [5 1]))

(width (rectangle [1 1] [1 1]))

(width (rectangle [3 1] [10 4]))


(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]]  rectangle]
      (- y2 y1)
  )
 )

  (height (rectangle [1 1] [5 1]))
  (height (rectangle [1 1] [5 5]))
  (height (rectangle [0 0] [2 3]))




(defn square? [rectangle]

   (= (height rectangle)  (width rectangle))

)

(square? (rectangle [1 1] [2 2]))
(square? (rectangle [1 1] [2 3]))
(square? (rectangle [1 1] [1 1]))


(defn area [rectangle]
  (* (height rectangle)  (width rectangle))
)

(area (rectangle [1 1] [5 1]))
(area (rectangle [0 0] [4 3]))


(defn contains-point? [rectangle point]
  :-)

(defn contains-rectangle? [outer inner]
  :-)


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


(defn title-length [book]
    (count (:title book))
  )

(title-length cities)
(title-length wild-seed)

(defn author-count [book]
  (count (:authors book))

  )

(author-count little-schemer)



(defn multiple-authors? [book]
  (> (author-count book) 1)
  )


(multiple-authors? little-schemer)

(assoc cities :awards ["Hugo", "World Fantasy Award",
                       "Arthur C. Clarke Award",
                       "British Science Fiction Award"])




(defn add-author [book new-author]
  (let [ na1 (conj (:authors book) new-author)]
     (println na1)
     (assoc book :authors na1)
    )
  )

(add-author little-schemer {:name "Gerald J. Sussman"})

(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})

(defn alive? [author]
  (not (contains? author :death-year))
  )

(alive? octavia )

(defn element-lengths [collection]
  (map count (seq collection))

  )

(element-lengths ["foo" "bar" "" "quux"])
(element-lengths ["x" [:a :b :c] {:y 42}])

(defn second-elements [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)
    )

  )

(second-elements [[1 2] [2 3] [3 4]])

(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])

(defn titles [books]
  (map :title books)

  )

(def books [cities, wild-seed, embassytown, little-schemer])

(titles books)

(defn monotonic? [a-seq]
   (if (apply <= a-seq)
     :else (apply >= a-seq))
  )

(monotonic? [1 2 3])
(monotonic? [0 1 10 11])
(monotonic? [3 2 0 -3])
(monotonic? [3 2 2])

(monotonic? [1 2 1 0])


(defn stars [n]
  (apply str (repeat n "*"))
)

(stars 7)

(defn toggle [a-set elem]
   (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
   )

(toggle #{:a :b :c} :d)
(toggle #{:a :b :c} :a)

(defn contains-duplicates? [a-seq]

   (not (=  (count a-seq) (count (set a-seq)) ))

  )

(contains-duplicates? [1 1 2 3 -40])
(contains-duplicates? [1 2 3 -40])
(contains-duplicates? [1 2 3 "a" "a"])


(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(defn old-book->new-book [book]

    (assoc book :authors (set (:authors book)))


  )



(old-book->new-book {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(old-book->new-book {:title "Wild Seed", :authors [octavia]})



(defn has-author? [book author]
    (contains? (:authors book) author)
  )

(contains? (:authors cities) china)
(has-author? cities china)
(has-author? cities felleisen)
(has-author? little-schemer octavia)

(defn authors [books]
   (apply concat (map :authors books))

  )

(authors [cities, wild-seed])

(defn all-author-names [books]
    (set (map :name (apply concat (map :authors books))))

  )


(all-author-names books)
(all-author-names [cities, wild-seed])
(all-author-names [])

(defn author->string [author]
  (let [nam (:name author)
        byr (:birth-year author)
        dyr (:death-year author)
        ]

    (let [yr (if byr (str " (" byr " - " dyr ")"))]
      (str nam  yr)
      )


    )

  )


(author->string felleisen)
(author->string friedman)
(author->string octavia)

(apply str (interpose " and " [1 2 3]))



(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))

  )


(authors->string (:authors little-schemer))
(authors->string #{octavia})
(authors->string #{})
(authors->string #{octavia, friedman})


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))

  )

(book->string wild-seed)
(book->string little-schemer)


(defn books->string [books]
  (cond
     (= (count books) 0 ) "No books"
     (= (count books) 1 ) (str "1 book. " (apply str (map book->string books)))
     :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))))
   )

  )

(books->string [])
(books->string [cities])
(books->string [little-schemer, cities, wild-seed])

(defn books-by-author [author books]
    ;(filter (filter (fn [x] (= (:name (first x)) (:name china) )) (map :authors books) ) books)

   ;(filter (fn [x] (= (count (:authors x)) 1)) books)

 ; (filter (fn[x] (= (:name (first x)) (:name author )) (map :authors books)))

  (filter (fn[x] (= (:name (first x)) "China Miéville" )) books)

  )


(filter (fn[x] (= (:name (first x)) "China Miéville" )) (map :authors books))

(println books)

(println (str books))

(books-by-author china books)

(map :authors books)

(set (map :authors books))




(contains? (set (map :name (map :authors books))) china )

(has-author? (map :authors books) china)

(books-by-author china books)

(def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
    (let [a (filter (fn [x] (= (:name x) name) ) authors)]
    (if (not (= 0 (count a)))
    (apply str (map :name a))))
  )


(author-by-name "Octavia E. Butler" authors)
(author-by-name "China Miéville" authors)

(author-by-name "Octavia E. Butler" #{felleisen, friedman})

(defn living-authors [authors]
    (let [a (filter alive? authors)]
       (if (not (= 0 (count a)))
     (seq (map :name a))))
  )

(living-authors authors)
(living-authors #{octavia})


(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})

(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})



(defn has-a-living-author? [book]
  (let [dth (filter (fn [x]  (nil? (:death-year x))  ) (:authors book))]

      (> (count dth) 0)


    )


  )

(has-a-living-author? deus-irae )
(has-a-living-author? silmarillion)
(has-a-living-author? wild-seed)
(has-a-living-author? little-schemer)
(has-a-living-author? cities)

(:authors wild-seed)



(defn books-by-living-authors [books]
  (map :title (filter (fn [x]  (has-a-living-author? x)) books))

  )


(books-by-living-authors books)

; %________%
