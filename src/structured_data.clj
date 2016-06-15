(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)
    )
  )

(defn spiff [v]
  (let [elem1 (get v 0)
        elem3 (get v 2)]
    (if (and  elem1 elem3)
      (+ elem1 elem3)
      "?"
    )
    )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
   (let [[x y z] v]
    (if (and  x z)
      (+ x z)
      "?"
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
    )
  )

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point
        ]
    (and (<= x1 xp x2) (<= y1 yp y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[left_bottom right_top] inner]
    (and (contains-point? outer left_bottom) (contains-point? outer right_top))
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (let [oldAuthors (:authors book)
        newAuthors (conj oldAuthors new-author)]
    (assoc book :authors newAuthors)
  )
  )

(defn alive? [author]
   (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1))]
    (map sec collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
   (not (== (count (set a-seq)) (count a-seq)))
  )

(defn old-book->new-book [book]
    (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
   (str (:name author) ( if(:birth-year author) (str " (" (:birth-year author) " - "  (:death-year author) ")" ) "" ) )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [bookNum (count books)
        strBookNum (str (if(== bookNum 0) "No" bookNum) " " (if(== bookNum 1) "book" "books"))       
        ]
    (str strBookNum (if(== bookNum 0) "" ". ") (apply str (interpose ". " (map book->string books))) ".")
    
    
    )
  )

(defn books-by-author [author books]
  (filter (fn [x](has-author? x author)) books) 
  )

(defn author-by-name [name authors]
  (first (filter (fn [x](= (:name x) name))  authors)) 
  
  )

(defn living-authors [authors]
  (filter alive?  authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
