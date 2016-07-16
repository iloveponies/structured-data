(ns structured-data)

(defn do-a-thing [x]
  (let  [t (+ x x)]
    (Math/pow t t)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[h1 w1] [h2 w2]] rectangle]
    (- h2 h1)
   ))

(defn height [rectangle]
  (let [ [[h1 w1] [h2 w2]] rectangle]
    (- w2 w1)
   ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (let [ [[h1 w1] [h2 w2]] rectangle]
    (* (- h2 h1 ) (- w2 w1))
    
)
  )

(defn contains-point? [rectangle point]
  (let
      [ [[h1 w1] [h2 w2]] rectangle
	  [x y ] point
	 ]
    (and
     (<= h1 x h2)
     (<= w1 y w2)
     )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [
;       [[oh1 ow1] [oh2 ow2]] outer
       [[ih1 iw1] [ih2 iw2]] inner
       ]
    (and
     (contains-point? outer (point ih1 iw1))
     (contains-point? outer (point ih1 iw2))
     (contains-point? outer (point ih2 iw1))
     (contains-point? outer (point ih2 iw1))
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
  ( let [aut (:authors book)]
   ( assoc book :authors
	   (conj  aut  new-author)
	   )
   )
  )

(defn alive? [author]
 (not  (contains? author :death-year) )
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)
      )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (cond
  (apply <= a-seq) true
  (apply >= a-seq) true
  :else false
   )
  )
  

(defn stars [n]
  (apply str  (repeat n "*")
   ) 
  )

(defn toggle [a-set elem]
  (if (not (contains? a-set elem)) (conj a-set elem) (disj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))
       ))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))
	 )
  )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (if (contains? author :birth-year)
    (str (:name author) " (" (:birth-year author) " - "( :death-year author)  ")")
    (:name author)
    )
  )

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))
 )

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))  )
  )

(defn books->string [books]
  (cond
  (=  (count books ) 0 ) "No books."
  (=  (count books ) 1 ) (apply str "1 book. "  (apply str (map book->string books)) ".")
  (>  (count books ) 1 ) (apply str (count books) " books. "
			     (apply str (interpose ". "  (map book->string books) ))
			      ". " )
  )
  )

(defn books-by-author [author books]
 (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let  [res     (filter (fn [x] (= name  (:name x))) authors) ]
    (cond
     (= (count res ) 0) nil
     (> (count res ) 0) (first res)
     )    
    )
)

(defn living-authors [authors]
; (set (map :name 
 (filter (fn [x] (alive? x)) authors)
;)
)

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true
      )
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )

; %________%
