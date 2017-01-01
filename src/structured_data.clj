(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
)
(defn spiff [v]
  (+ (get v 0) (get v 2))



  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))

  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (= (width rectangle)(height rectangle))


)

(defn area [rectangle]
  (* (width rectangle)(height rectangle))

  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [pointx pointy] point]
    (and (<= x1 pointx x2) (<= y1 pointy y2)))
  )

(defn contains-rectangle? [outer inner]
   (let [[eka toka] inner]

   (and (contains-point? outer eka) (contains-point? outer toka))

   )


)

(defn title-length [book]
  ;;(let [ temp  (:title book) ]
  ;; count('(temp)))
  (count(:title book))

  )

(defn author-count [book]

  (count(:authors book))

  )

(defn multiple-authors? [book]
  (> (author-count book) 1)

  )

(defn add-author [book new-author]
  (let [ temp (:authors book)]
    (assoc book :authors (conj temp new-author))

  )


  )

(defn alive? [author]
  (not (contains? author :death-year))


  )

(defn element-lengths [collection]
  (map count collection)

  )

(defn second-elements [collection]
  (let [oma (fn [col] (get col 1))]
    (map oma collection))

  )

(defn titles [books]

  (map :title books)

  )

(defn monotonic? [a-seq]

;;   (apply <= a-seq)

   (or (apply <= a-seq) (apply >= a-seq))

  )

(defn stars [n]
  (apply str(repeat n "*"))

  )

(defn toggle [a-set elem]

  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )

)

(defn contains-duplicates? [a-seq]

  (not (= (count a-seq) (count (set a-seq))))

)

(defn old-book->new-book [book]

  (assoc book :authors (set (:authors book)))


  )

(defn has-author? [book author]

  (contains? (:authors book) author)

  )

(defn authors [books]
  ;;  ensin authors sitten vasta name seuraavassa metodissa
  (apply clojure.set/union (map :authors books))

  )

(defn all-author-names [books]
  (set (map :name (authors books)))

  )

(defn author->string [author]

  (let [name (:name author)
   by (:birth-year author) dy (:death-year author)]
   (if (nil? by) ;; jos ei syntyv. silloin ei kumpaakaan
    (str name)
    (str name " (" by " - " dy ")")
   )
  )

  )

(defn authors->string [authors]


(apply str (interpose ", " (map author->string authors)))


  )

(defn book->string [book]
  (let [name (:title book)]

    (str name ", written by " (authors->string (:authors book)))


  )

  )

(defn books->string [books]
  (let [laskuri (count books)]
   (cond
		(== laskuri 1) (str"1 book. " (apply str (interpose ". " (map book->string books))) ".")
		(> laskuri 1)  (str laskuri " books. " (apply str (interpose ". " (map book->string books))) ".")
    :else (str "No books.")
	 )


  )
  )

(defn books-by-author [author books]

  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
 ;;(let [temp (:name x)]
  (first(filter (fn [x] (= name (:name x))) authors))
 ;;)

  ;; testit menee läpi, instarepl ongelma
  )

(defn living-authors [authors]

  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))

  )

(defn books-by-living-authors [books]

  (filter has-a-living-author? books)


  )

; %________%
