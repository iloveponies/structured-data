(ns structured-data)


(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  ))

(defn spiff [v]
  (+ (get v 0 ) (get v 2)))

(defn cutify [v]
  (conj v "<3")
  )



(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)
  ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)
  ))




(defn square? [rectangle]
  (== (height rectangle) (width rectangle) )
  )



(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


(defn contains-point? [rectangle point]

  (let [[[x1 y1] [x2 y2]] rectangle]

    (and (<= x1 (get point 0) x2) (<= y1 (get point 1) y2) )

  ))


      ;=> true

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (get inner 0)) (contains-point? outer (get inner 1)) )
  )



(defn title-length [book]
  (count (get book :title)))



(defn author-count [book]
  (count (get book :authors)))



(defn multiple-authors? [book]
  (> (author-count book) 1 ))


(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))

   ))



(defn alive? [author]
  (if (author :death-year nil)
    false
    true
  ))


(defn element-lengths [collection]
  (map count collection)

  )


(defn second-elements [collection]
  (let [helper (fn [x] (get x 1))]
    (map helper collection)
    )
  )


(defn titles [books]
  (map :title books))



(defn monotonic? [a-seq]

  (or (apply <= a-seq) (apply >= a-seq) )

  )


(defn stars [n]
  (apply str (apply concat (repeat n "*")))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )

  )

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq)  ))

  )


(defn old-book->new-book [book]
  (let [setti (book :authors)]
    (assoc book :authors (set setti))

    )

  )



(defn has-author? [book author]
  (contains? (book :authors) author)
  )

(defn authors [books]

          (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set(map :name (authors books))))


(defn author->string [author]
  (let [name (author :name)
        year1 (str " ("(author :birth-year) " - ")
        year2 (str  (author :death-year))
        ]
    (if (not (author :birth-year))
        (str name)

        (str name year1 year2 ")")
      )
      )


    )



(defn authors->string [authors]
  (apply str (clojure.set/union (interpose ", "(map author->string authors))))

  )



(defn book->string [book]
  ;(let [name (book :name) writer (author->string book)]
  ;  (apply str (interpose ", written by" ()))
;
 ;   )

  (str (book :title) ", written by " (authors->string (book :authors)))

  )




(defn books->string [books]
  (if (empty? books)
    "No books."
    (if (== (count books) 1)
      (str "1 book. " (apply str (map book->string books)) ".")

      (str  (count books) " books. " (apply str (interpose ". "(map book->string books)))".")
    )
  ))




(defn books-by-author [author books]
  ;(filter #(has-author? books author ) books)
  (let [hasAuthorInBook (fn [book] (contains? (book :authors) author))]
    (filter hasAuthorInBook books)
  ))



(defn author-by-name [name authors]
  (let [checkAuthor (fn [author] (= (author :name) name) )]
    (first (filter checkAuthor authors))

    )
)
        ;=> nil


(defn living-authors [authors]
  (let [checkAlive (fn [author] (alive? author))]
    (filter checkAlive authors)

    )

  )

;=> (china, felleisen)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors))))


  )



(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


;=> (little-schemer cities embassytown silmarillion)


; %________%
