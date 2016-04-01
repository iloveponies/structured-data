(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)

  )
  )

(defn spiff [v]
  (if (< (count v) 3) "?"

    (+ (get v 0) (get v 2))
    )
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
    (if (< (count v) 3) "?"
    (let [ [x y z] v ]
       (+ x z)
      )
   )
  )

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
    ))

(defn square? [rectangle]
   (= ( width rectangle) (height rectangle ) )
  )

(defn area [rectangle]

  ( * ( width rectangle) (height rectangle ) )

  )

(defn contains-point? [rectangle point]
  (let  [[[x1 y1] [x2 y2]] rectangle [x y] point ]
       (and (<= x1 x x2 ) (<= y1 y y2 ) )
    ))

(defn contains-rectangle? [outer inner]
  (let [ [point1 point2] inner]
       (and ( contains-point? outer point1 )
            (contains-point? outer point2 ) )
    )

  )

(defn title-length [book]
  (count (:title book ) )
  )

(defn author-count [book]
 (count (:authors  book ) ))

(defn multiple-authors? [book]
  (>(author-count book ) 1))

(defn add-author [book new-author]
  ( let [authors (:authors book)  new-authors (conj authors new-author )]
  (assoc book :authors new-authors  )
    ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [ second (fn [v] (get ( v 1 )) ) ])
    (map second collection)
  )

(defn titles [books]
  (map :title books  ))

(defn monotonic? [a-seq]
  ( or  (apply <= a-seq) (apply >= a-seq ) )
  )

(defn stars [n]
  (apply str (repeat n "*") ))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)

  ))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  ( assoc book :authors (set ( :authors book )) )
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union ( map :authors books ))
  )

(defn all-author-names [books]
 (set (map :name (authors books) ))
  )

(defn author->string [author]
  ( let [ {name :name by :birth-year
           dy :death-year } author
          life (if (and (= by nil) (= dy nil ) ) ""
                 (str " (" by " - " dy ")" ) )]

     (str name life )

  )
  )

(defn authors->string [authors]
 (apply str (interpose, ", "
        (map author->string authors)))
  )

(defn book->string [book]
   (let [ {t :title  a :authors } book]

     ( str t ", written by " (authors->string a ) )

     )

  )

(defn books->string [books]
   ( if (empty? books) "No books."
    (str (count books )
        ( if (= (count books) 1 )  " book. " " books. ")
        (apply str (interpose, ". "
        (map book->string  books))) "." )
     )

  )

(defn books-by-author [author books]
   (let [has-author-f? (fn[x] ( has-author? x author ) ) ]
      (filter has-author-f? books)
     )
  )

(defn author-by-name [name authors]
   (let [ a (filter (fn[x]  (= ( :name x ) name ))  authors )]

     (if  (empty? a)  nil (first a) )

     )
  )

(defn living-authors [authors]
    (filter alive? authors)

  )

(defn has-a-living-author? [book]
   (not (empty? (living-authors (:authors book ))))

  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books )

  )

; %________%
