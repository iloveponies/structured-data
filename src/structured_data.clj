(ns structured-data)

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

;(def authors #{china, felleisen, octavia, friedman})

(defn add-author [book author]
      (assoc book :authors (conj (:authors book) author))
)

(defn old-book->new-book [book]
      (assoc book :authors (set (:authors book) ))
)

(defn do-a-thing [x]
  (let [xx (+ x x)]
   (Math/pow xx xx))
)


(defn spiff [input]
  (+ (get input 0) (get input 2))
 )
(defn spiff-destructuring [input]
  (let [[x y z] input]
  (+ x z)
  )
)

(defn cutify [input]
  (conj input "<3")
  )

(defn point [x y]
  [x y]
)

(defn rectangle [bottom-left top-right]

  [bottom-left top-right])

(defn height [box]
        (- (get (get box 1) 1) (get (get box 0) 1))
        )

(defn width [box]
        (- (get (get box 1) 0) (get (get box 0) 0))
        )

(defn square? [box]
 (= (height box) (width box))
 )

(defn area [box]
  (* (height box) (width box))
)

(defn contains-point? [box point]
  (and (<= (get (get box 0) 0) (get point 0) (get (get box 1) 0))  (<= (get (get box 0) 1) (get point 1) (get (get box 1) 1)))
 )

(defn contains-rectangle? [outter inner]
      (and (contains-point? outter (get inner 0)) (contains-point? outter (get inner 1)))
)

(defn title-length [book]
  (count (:title book ))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (not (= (count (:authors book)) 1))
  )

(defn alive? [person]
  (= nil (:death-year person))
  )

(defn element-lengths [elements]
  (map count elements)
  )

(defn second-elements [elements]
  (let [se (fn [element] (get element 1))]
    (map se elements)
    )
  )



(defn titles [books]
  (seq (map :title books))
  )

(defn stars [times]
  (apply str (repeat times "*"))
  )



(defn monotonic? [input]
  (or (apply <= input) (apply >= input))
  )

(defn toggle [s e]
  (if (contains? s e) (disj s e) (conj s e))
  )

(defn contains-duplicates? [s]
  (< 0 (- (count s) (count (set s))))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn all-author [books]
  (let [author
         (fn [book] (:authors book))]
    (set (apply concat (map author books)))))

(defn authors [books]
  (all-author books)
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
 (apply str (:name author)  (if (contains?  author :birth-year) (str " ("  (:birth-year author) " - ")  "")
        (if (contains?  author :death-year) (:death-year author)  "")  (if (contains?  author :birth-year) ")" "") )
)


(defn author->string [author]
 (apply str (:name author)  (if (contains?  author :birth-year) (str " ("  (:birth-year author) " - ")  "")
        (if (contains?  author :death-year) (:death-year author)  "")  (if (contains?  author :birth-year) ")" "") )
)

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn titles [books]
  (seq (map :title books))
  )

(defn books->string [books]
  (cond
   (= 0 (count books)) "No books."
   (= 1 (count books)) (str "1 book. " (book->string (get books 0)) ".")
   :else (str (count books) " books. "
              ( let [one-str (fn [book] (str (book->string book) ". "))]
                (apply str (seq (map one-str books))))
    )
   )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author))  books)
  )

(defn author-by-name [name authors]
   (first (seq (filter (fn[author] (=  name (:name author)))  authors)))
)

(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})

(def christopher {:name "Christopher Tolkien" :birth-year 1924})

(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})

(def christopher {:name "Christopher Tolkien" :birth-year 1924})

(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})

(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})

(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn living-authors [authors]
  (filter (fn [author] (not (contains? author :death-year))) authors)
)

(defn has-a-living-author? [book]
  (contains? (set
    (map (fn [author] (not (contains? author :death-year))) (:authors book)) ) true)
)

(defn books-by-living-authors[books]
  (filter has-a-living-author? books)
  )
