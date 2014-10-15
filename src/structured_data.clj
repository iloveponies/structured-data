(ns structured-data)

(defn do-a-thing [x]
 (let [double_x (+ x x)]
  (Math/pow double_x double_x)
  )
 )

(defn spiff [v]
  ( + (get v 0) (get v 2) )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
    )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (= (height rectangle) (width rectangle)))
  )

(defn area [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (* (height rectangle) (width rectangle)))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
       [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[op1 op2] outer
        [ip1 ip2] inner]
    (and (contains-point? outer ip1) (contains-point? outer ip2)))
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
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author)))
  )

(defn alive? [author]
  (if (contains? author :death-year) false
    true)
  )

(defn element-lengths [collection]
  (map count (seq collection))
  )

(defn second-elements [collection]
  (let [get-second (fn [coll] (first (rest coll)))]
    (map get-second collection)
    )
  )

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq ) true 
    (apply >= a-seq ) true 
    :else false)
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) 
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq)) ) false true)
    )

(defn old-book->new-book [book]
  (assoc book :authors (set(:authors book)))
)

(defn has-author? [book author]
  ;;(contains? (:authors old-book->new-book book) author)
  (contains? (:authors book) author)
   )

(defn authors [books]
   (apply clojure.set/union (concat (map :authors books)))
)

(defn all-author-names [books]
   (set (map :name (authors books)))
)

(defn author->string [author]
  (let [authorname (:name author)
        years (if (contains? author :birth-year) 
                (apply str(concat " (" (str(:birth-year author)) " - "
                  (if (contains? author :death-year)
                    (str(:death-year author)) 
                  )
                ")")
                )
              )
        ]
        (apply str(concat authorname years))
      )
  )
        
              
(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors)))
  )

(defn book->string [book]
    (let [book-title (apply str (concat (str (:title book)) ", written by "))
          author-string (authors->string (:authors book))]
          (apply str 
                 (concat 
                   (str book-title) 
                   (str author-string) 
                   )
                 )
      )
  )

(defn books->string [books]
  (let [book-count
        (if (= 0 (count books)) "No books."
          (if (= 1 (count books)) "1 book."
            (apply str (concat (str (count books) " books. ")))
        ))
        ]
    (if (> (count books) 0 )
      (apply str
        
          (concat 
            (str book-count) 
              " "
              (apply str 
                     (interpose ". " 
                                (map book->string books)
                        )
                ) "."
            )   
        )
         (str book-count) 
      )
    )
  )

(defn books-by-author [author books]
  (filter  #(has-author? % author) books)
   )


(defn author-by-name [name authors]
  (first (filter #(= 
     (:name %) name)
      authors 
      )
    )
  )

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
