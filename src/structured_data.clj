(ns structured-data)

(defn do-a-thing [x]

	(let [jee (+ x x)]
    (Math/pow jee jee)))

(defn spiff [v]

  	
 	(+ (get v 0) (get v 2))


  )

(defn cutify [v]

	(conj v "<3")

  )

(defn spiff-destructuring [v]
  (let[[x y z] v]
	(+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [ [[x1 y1][x2 y2]] ]

  	(- x2 x1))

(defn height [ [[x1 y1][x2 y2]] ]
  	(- y2 y1))

(defn square? [ [[x1 y1][x2 y2]] ]

  (= (- x2 x1) (- y2 y1)))

(defn area [rectangle]


    (* (height rectangle) (width rectangle))

  )

(defn contains-point? [[[x1 y1][x2 y2]] [pointX pointY]]

  (if (and (<= x1 pointX x2) (<= y1 pointY y2))
    true
    false

  ))

(defn contains-rectangle? [[[ox1 oy1][ox2 oy2]] [[ix1 iy1][ix2 iy2]]]

	(if (and 
         (and (>= ix1 ox1)(>= iy1 oy1)) 
         (and (<= ix2 ox2) (<= iy2 oy2)))
		true
		false
      ) 

  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]

  (count (:authors book) 

  ))

(defn multiple-authors? [book]

	(if (> (count (:authors book)) 1)
      true
      false
      )
  )

(defn add-author [book new-author]

  (let [uudet (:authors book)]
  (assoc book :authors (conj uudet new-author))


  ))

(defn alive? [author]

  (if (:death-year author)
    false
    true
  ))

(defn element-lengths [collection]

	(map count collection)
  )

(defn second-elements [collection]

  (map second collection)

  )

(defn titles [books]

  (map :title books)

  )

(defn monotonic? [a-seq]

  (if (apply <= a-seq)
    true
    (if (apply >= a-seq)
      true
      false)
    )
  )

(defn stars [n]

  (apply str(repeat n "*"))

  )

(defn toggle [a-set elem]

  (if (contains? a-set elem)
	(disj a-set elem)
    (conj a-set elem)

  ))

(defn contains-duplicates? [a-seq]

  (if (== (count a-seq)(count (set a-seq)))
	false
	true
    )

  )

(defn old-book->new-book [book]

 	(assoc book :authors (set (:authors book)))

  )

(defn has-author? [book author]

  (contains? (:authors (old-book->new-book book)) author)
  )

(defn authors [books]


  (apply clojure.set/union (map :authors books))

  )

(defn all-author-names [books]

  (set (map :name (authors books)))

  )

(defn author->string [author]
 (let [{name :name 
        birth :birth-year 
        death :death-year} author]

   (if (contains? author :death-year)
     (str name " (" birth " - " death ")")
     (if (contains? author :birth-year)
		(str name " (" birth " - )")
        name
     ))))

(defn authors->string [authors]

  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]


	(str (:title book) ", written by " (authors->string (:authors book)))



  )

(defn books->string [books]

(if (< (count books) 1)
      "No books."
  	  (if (< (count books) 2)
        (str "1 book. " (book->string (first books)) ".")
		(str (count books) " books. " (apply str(interpose ", "(map book->string books))) ".")))



  )

(defn books-by-author [author books]


  (filter (fn [x] (has-author? x author) ) books)

)

(defn author-by-name [name authors]

	(first (filter (fn [x] (= (:name x) name))authors))

  )

(defn living-authors [authors]

	(filter (fn [x] (alive? x)) authors)

  )

(defn has-a-living-author? [book]

  (if (empty? (living-authors (:authors book)))
    false
    true


  ))

(defn books-by-living-authors [books]


  (filter (fn [x] (has-a-living-author? x)) books)
  )