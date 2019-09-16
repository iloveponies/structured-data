(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a ))
 )



(defn spiff [v]
  (let [f (get v 0)
        third (get v 2)]
  (cond
   (= f nil) nil
   (= third nil) f
   :else (+ f third)
    )))

(defn cutify [v]
 (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v]
  (cond
   (= a nil) nil
   (= c nil) a
   :else (+ a c)
    )))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)
 )
)


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
   )
 )

(defn square? [rectangle]
  (== (width rectangle)(height rectangle))
)

(defn area [rectangle]
  (* (width rectangle)(height rectangle))
)

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
          [x y] point ]

    (and (<= x1 x x2) (<= y1 y y2) )
    )
 )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (and
    (contains-point? outer [x1 y1])
    (contains-point? outer [x2 y2])
     )
  )
)

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
 (count (:authors book))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (let [oldAuthors (:authors book)]
    (assoc book :authors
    (conj oldAuthors new-author))
    )
  )

(defn alive? [author]
 (not(contains? author :death-year))
 )

(defn element-lengths [collection]
 (map count collection))

(defn second-elements [collection]

  (let [secvec (fn [v] (get v 1))]
    (map secvec collection))
 )

(defn titles [books]
  (map :title books)
 )

(defn monotonic? [a-seq]
 (or (apply <= a-seq)
     (apply >= a-seq)
  )
 )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
   )
)

(defn contains-duplicates? [a-seq]
  (not(== (count a-seq)(count (set a-seq))))
  )

(defn old-book->new-book [book]
  (let [v_aut (:authors book)]
 (assoc book :authors (set v_aut))
    )
 )

(defn has-author? [book author]
  (let [authors (:authors (old-book->new-book book))]
    (contains? authors author))
  )

(defn authors [books]
  (let [getAuthor (fn[b] (:authors b))]

    (apply clojure.set/union (map getAuthor books ))

  )
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
 )

(defn author->string [author]
  (let [nam (:name author)
        by (:birth-year author)
        dy (:death-year author)]

  (if (nil? by)
      ( str nam nil)
       (str nam " (" by " - " dy ")"))

  )

  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
 )



(defn book->string [book]
  (let [title (:title book)
        ats (:authors book)]
 (str title ", written by " (authors->string ats)  )

    )
 )

(defn books->string [books]
   (cond
   (empty? books) "No books."
   (== 1 (count books)) (str "1 book. " (book->string (first books)) ".")
   (>  (count books) 1)

       (str (count books) " books. "
            (apply str (interpose ". " (map book->string books))) "."

                   )
   )
 )

(defn books-by-author [author books]
 (filter (fn [book](has-author? book author)) books)
)

(defn author-by-name [nam authors]

    (let [head (first authors)

        tail (rest authors)
        ]

    (cond

     (empty? head) nil
     (= nam (:name head)) head

      :else (author-by-name nam tail)


      )

    )

  )

(defn living-authors [authors]
  (filter alive? authors)
  )


(defn has-a-living-author? [book]

  (let [ats (authors [book])
        dys (set(map :death-year ats))]



  (contains?  dys nil))
 )



(defn books-by-living-authors [books]

  (filter has-a-living-author? books)
)

; %________%
