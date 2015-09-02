(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx) ) )

(defn spiff [v]
  (+ (get v 0) (get v 2) ) )

(defn cutify [v]
  (conj v "<3") )

(defn spiff-destructuring [v]
  (let [ [x y z] v ]
    (+ x z) ) )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (- x2 x1) ) )

(defn height [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (- y2 y1) ) )

(defn square? [rectangle]
  (let [h (height rectangle)
        w (width rectangle)] 
     (= 0 (- h w) ) ) )

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)
        area (* h w)] 
     (if (< area 0) (* -1 area) area ) ) )

(defn contains-point? [rectangle point]
  (let [ [ [x1 y1] [x2 y2] ] rectangle
         [ xp yp ] point ] 
    (if (and (<= x1 xp x2) (<= y1 yp y2) ) true false
    )
  )
)

(defn contains-rectangle? [outer inner]
  (if (and (contains-point? outer (inner 0)) 
           (contains-point? outer (inner 1)) )
      true false
  )
)

(defn title-length [book]
  (count (get book :title) )
)

(defn author-count [book]
  (count (:authors book) )
)

(defn multiple-authors? [book]
  (if (> (author-count book) 1) 
    true false
  )
)

(defn add-author [book new-author]
  (let [ {:keys [authors]} book 
       ]
    (assoc book :authors (conj authors new-author) )
  )
)

(defn alive? [author]
  (if (contains? author :death-year) false true
  )
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [secondDimension (fn [x] (get x 1))]
    (map secondDimension collection))
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
    (cond (apply <= a-seq) true
      :else
      (if (apply >= a-seq) true false)
    )   
)

(defn stars [n]
  (apply str (apply repeat n (concat (str "*"))))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq))) false true
  )
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book) ) )
)

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false)
)

(defn authors [books]
  (let [author-names
         (fn [book] (set (:authors book)))]
    (apply clojure.set/union (map author-names books) )
  )
)

(defn all-author-names [books]
  (set (map :name (authors books)) )
)

(defn author->string [author]
  (let [nimi (:name author)
        elinaika (str "(" (:birth-year author) " - "
                      (:death-year author) ")"
                 )
       ]
    (cond (> (count (str (:birth-year author))) 0)
      (str nimi " " elinaika)
      :else (str nimi)
    )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (for [x authors] (author->string x))))
)

(defn book->string [book]
  (let [otsikko (:title book)
        kirjailijat (set (:authors book))
       ]
  (str otsikko ", written by " (authors->string kirjailijat) )
  )
)

(defn books->string [books]
  (let [laskuri (count books)
       ]
    (cond (= 0 laskuri) "No books."
    :else (cond (= 1 laskuri) 
            (str "1 book. " (book->string (books 0)) ".")
          :else (str laskuri " books" 
                 (apply str(for [x (range 0 laskuri)] 
                 (str ". " (book->string (books x)) ))) ".")
          )
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors))
)

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
