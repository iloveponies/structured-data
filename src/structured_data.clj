(ns structured-data)

(defn do-a-thing [x]
(let [z (+ x x )]
  (Math/pow z z )
  ))


(defn spiff [v]
  (if (< (count v) 3)
    (str "?")
    (+ (get v 0) (get v 2)))
  )

(defn cutify [v]
    (conj v (str "<3"))
  )

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
 (let [[[x1 y1][ x2 y2]] rectangle]
    (- x2 x1)
  )
  )
(defn height [rectangle]
 (let [[[x1 y1][ x2 y2]] rectangle]
    (- y2 y1)
  )
  )
(defn square? [rectangle]
 (let [[[x1 y1][ x2 y2]] rectangle]
    (if (== (- x1 x2) (- y1 y2))
         true
        false
      )
  )
)

(defn area [rectangle]
   (let [[[x1 y1][ x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))
  )
)


(defn contains-point? [rectangle point]
  (let [[[x1 y1][ x2 y2]] rectangle [p1 p2] point]
      (and (<= x1 p1 x2) (<= y1 p2 y2))
  )
)

(defn contains-rectangle? [outer inner]
   (let [[[ox1 oy1][ ox2 oy2]] outer [[ix1 iy1][ix2 iy2]] inner]
      (and (contains-point? outer [ix1 iy1]) (contains-point? outer [ix2 iy2] )
  )
)
)

(defn title-length [book]
   (count (get book :title))
  )

(defn author-count [book]
  (count (get book :authors))
)

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1)
        true
        false
    )
)
(defn add-author [book new-author]
 (let[old-authors (get book :authors)]
   (assoc book :authors (conj old-authors new-author))
  )
)

(defn alive? [author]

  (if (contains? author :death-year)
  false
  true)
)





(defn element-lengths [collection]
 (map count collection)
)

(defn second-elements [collection]
    (let [nappaa (fn[x] (get x 1))]
    (map nappaa collection)
  )
)

(defn titles [books]
(map :title books)
)

(defn monotonic? [a-seq]
   (if (apply <= a-seq)
  true
   (if ( apply >= a-seq)
  true
     false)
)
  )

(defn stars [n]
(apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if(contains? a-set elem)
     (disj a-set elem)
     (conj a-set elem)
  )
  )

(defn contains-duplicates? [a-seq]
    (if (not= (count a-seq) (count (set a-seq)))
      true
      false
      )
    )

(defn old-book->new-book [book]
    (assoc book :authors (set (book :authors))

  )
)
(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false
    )
)

(defn authors [books]
  (let [author-names
        ;;(fn[book] (map :name (:authors book)))]
       (fn[book] (:authors book))]
    (set (apply concat (map author-names books))))
  )

(defn all-author-names [books]
     (set (map :name (authors books))
    )
    )
(defn author->string [author]
    (let [name (:name author)
    byear (:birth-year author)
    dyear (:death-year author)]
     (if (contains? author :birth-year)
        (str name  " (" byear " - " dyear ")")
        (str name )
     )
    )
)
(defn authors->string [authors]
 (apply str (interpose ", "  (map author->string authors)))
  )


(defn book->string [book]
  (str (:title book) ", written by "  (authors->string(:authors book)))

  )
(defn dotter [book]
  (str (book->string book) ". ")
  )


(defn books->string [books]
  (cond (= (count books) 0) (str "No books.")
        (= (count books) 1) (str "1 book. " (:title (first books)) ", written by " (authors->string(:authors (first books) ) )".")
        (= (count books) 2) (apply str (concat "2 books. " (map dotter books) ))
;        (= (count bookes) 2) (apply str (concat "2 books. " (map book->string bookes) ))

        (= (count books) 3) (apply str (concat "3 books. " (map dotter books)))

  )
  )
(defn books-by-author [author books]
 (filter (fn [books] (has-author? books author)) books)
)
(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
