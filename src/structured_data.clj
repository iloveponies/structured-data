(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
     (Math/pow xx xx)
        )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)
     ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] 
	(+ x z)
    )
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
     (- y2 y1)
   ))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
   )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
   )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [pointx pointy] point]
     (and (<= x1 pointx x2) (<= y1 pointy y2))
   ))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) 
         (contains-point? outer point2))
    )
  )

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (> (count (get book :authors)) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author))

  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count
       collection)
  )

(defn second-elements [collection]
	(let [getsec (fn [vectr] (get vectr 1))]
  (map getsec
	collection
   )))

(defn titles [books]
  (let [gettitle (fn [book] (get book :title))]
   (map gettitle books))
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
  	(apply >= a-seq))
 )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
	(if (contains? a-set elem) 
      (disj a-set elem)
      (conj a-set elem)
      )
  )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors)) ))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [getauthors (fn [book] (get book :authors))]
	(apply clojure.set/union (map getauthors books))))

(defn all-author-names [books]
  (let [getnames (fn [author] (get author :name))]
	(set (map getnames (authors books))) ))

(defn author->string [author]
  (let [name (get author :name)]
    (if (contains? author :death-year)
    (str name " (" (get author :birth-year) " - " 
                   (get author :death-year) ")")
    (if (contains? author :birth-year)
      (str name " ("(get author :birth-year) " - )")
      (str name)
      )
    )
  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (get book :title) ", written by " 
       (authors->string (get book :authors)) ))

(defn books->string [books]
  (let [qnt (count books)]
  (cond 
   (== qnt 0) "No books."
   (== qnt 1) (str "1 book. " 
       (apply str (interpose ", " (map book->string books)))
       "."
       )
   :else (str qnt " books. " 
       (apply str (interpose ", " (map book->string books)))
       "."
       ))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first 
   (filter 
    (fn [author] (= (get author :name) name)) authors)))

(defn living-authors [authors]
  (filter 
    alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))
          ))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)