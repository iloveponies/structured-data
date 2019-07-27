(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
   (Math/pow xx xx)  
   )  
  )

(defn spiff [v]
  (+ (get v 0)
     (get v 2)
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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))
   )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))
   )
  )

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)
       ]
       (= w h)
  )
  )

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)
       ]
       (* w h)
  )
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
      (and 
        (<= x1 x x2)
        (<= y1 y y2)
      )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
      (and
        (contains-point? outer (point x1 y1))
        (contains-point? outer (point x2 y2))
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
  (let [author-count (count (get book :authors))]
    (> author-count 1)
  )
  )

(defn add-author [book new-author]
  (let [authors (get book :authors)
       new-authors (conj authors new-author)]
    (assoc book :authors new-authors)
  )
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)
  )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq))
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
  (not (= (count a-seq) (count (set a-seq))))
 )

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors
      (set authors))
  )
  )

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)
  )
  )

(defn authors [books]
  (set (apply concat (map :authors books)))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (get author :name)
        years
          (if (contains? author :death-year)
            (str " (" (get author :birth-year) " - " (get author :death-year) ")")
            (if (contains? author :birth-year)
             (str " (" (get author :birth-year) " - )")
             ""
            )
          )
    ]
    (str name years)
  )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str 
    (get book :title)
    ", written by "
    (authors->string (get book :authors))
  )
  )

(defn books->string [books]
  (let [book-count (count books)]
    (cond
      (= book-count 0)
        (str "No books.")
      (= book-count 1)
        (str "1 book. " (book->string (get books 0)) ".")
      :else
        (str book-count " books. " 
          (apply str (interpose ". " (map book->string books) ) )
          "."
        )
    )
  )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first 
    (filter (fn [author] (= name (get author :name))) authors)
  )
  )

(defn living-authors [authors]
  (filter (fn [author] (not (contains? author :death-year))) authors)
  )

(defn has-a-living-author? [book]
  (let [authors (get book :authors)]
    (> (count (living-authors authors)) 0)
  )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
