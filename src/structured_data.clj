(ns structured-data)

(defn do-a-thing [x]
  (let[z (+ x x)]
    (Math/pow z z)
  
  )
)
(defn spiff [v]
  (+ (get v 0) (get v 2)
  )
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
  
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
   )
  
  )

(defn height [rectangle]
  
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
  
  )
)

(defn square? [rectangle]
  
  (if (= (width rectangle) (height rectangle))
    
    true

    false
    
    )
  
  )

(defn area [rectangle]

  (* (width rectangle) (height rectangle))  
  
)

(defn contains-point? [rectangle point]
  
  (let [[[x1 y1][x2 y2]] rectangle [x3 y3] point]
    (and(<= x1 x3 x2)(<= y1 y3 y2))

   )
  
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))

(defn title-length [book]
  
  (count (:title book))
  
  )

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if(< 1 (author-count book)) true
        false))

(defn add-author [book new-author]
    
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors))
  
)  

(defn alive? [author]
  (not(boolean (:death-year author))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsec (fn [v] (get v 1))]
    (map getsec collection)
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or(apply <= a-seq )
     (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if(contains? a-set elem) (disj a-set elem)
    (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]  
    (if(= (count a-set) (count a-seq)) false
      true
    
    )))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)  
  )
  )
(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
