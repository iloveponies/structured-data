(ns structured-data)

(defn do-a-thing [x]
  (let [add (+ x x)]
      (Math/pow add add)
  )
)

(defn spiff [v]
   (let [fs (get v 0)
         th (get v 2)
        ]
       (+ fs th)
    )
)

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
   (let [[f s t] v]
      (+ f  t)
   )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] [(first rectangle) (second rectangle)]]
     (- x2 x1)
  )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] [(first rectangle) (second rectangle)]]
     (- y2 y1)
  )
)

(defn square? [rectangle]
    (== (height rectangle) (width rectangle))
)

(defn area [rectangle]
  (*(height rectangle) (width rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] [(first rectangle) (second rectangle)]
        [x3 y3] [(first point) (second point)]
        ]
     (and (<= x1 x3 x2) (<= y1 y3 y2))
  )
)

(defn contains-rectangle? [out [p3 p4]]
  (and (contains-point? out p3) (contains-point? out p4))
)

(defn title-length [book]
   (let [tit (:title book)]
     (count tit) 
   )
)

(defn author-count [book]
  (let [auth (:authors book)]
     (count auth) 
   ))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false)
)

(defn add-author [book new-author]
  (let [[oldAuth] [(:authors book)]
        [newauthors] [(conj oldAuth new-author)]
       ]
    (assoc book :authors newauthors)
  )
)

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [y] (count y)) collection)
)

(defn second-elements [collection]
  (let [seco (fn [y] (get y 1))]
      (map seco collection)
  )
)

(defn titles [books]
  (map :title books)  
)

(defn monotonic? [a-seq]
  (let [com (fn [f] (apply f a-seq))] 
   (or (com >=) (com <=))
  )
)

(defn stars [n]
  (apply str (repeat n \*))
  )

(defn toggle [a-set elem]
  (let [added (conj a-set elem) deleted (disj a-set elem)]
    (if (contains? a-set elem) deleted added)
  )
)

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq))))
)

(defn old-book->new-book [book]
  (let [oldAuth (:authors book)
        newAuth (set oldAuth)]
     (assoc book :authors newAuth)
   )
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
   (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (let [auth (authors books)
        names (map :name auth)]
    (set names)
  )
)

(defn author->string [author]
  (let [n (:name author)
       by (if (contains? author :birth-year) 
               (str " (" (:birth-year author) " - ")
               (str "")
          )
       dy (if (contains? author :death-year) 
               (str (:death-year author))
               (str "")
          )
       lastSign (if (contains? author :birth-year) 
                    (str ")")
                    (str "")
                )    
       ]
    (str n by dy lastSign)
  )
  )

(defn authors->string [authors]
  str (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [ti (:title book) 
        authors (authors->string (:authors book))
       ]
     (str ti ", written by " authors)
  )
)

(defn books->string [books]
  (let [numBooks (count books)
        strnum (str numBooks (if (== numBooks 1) " book. " " books. "))
        bookInfo (str (apply str (interpose ". " (map book->string books))))
        ]
    (if (empty? books) "No books." (str strnum bookInfo "."))
   ))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books)
  )

(defn author-by-name [n authors]
   (let [li (filter (fn [aut] (.equals (:name aut) n)) authors)]
     (first li)
   )
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )