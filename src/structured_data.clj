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
  (let [[a b c] v]
    (+ a c)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- x2 x1)
    )
  )

(defn height [rectangle]
   (let  [ [[x1 y1]  [x2 y2]] rectangle ]
      (- y2 y1)
      )
  )

(defn square? [rectangle]
   (let  [ [[x1 y1]  [x2 y2]] rectangle ]
      (if (== (- x2 x1) (- y2 y1)) true false )
      )
  )

(defn area [rectangle]
   (let  [ [[x1 y1]  [x2 y2]] rectangle ]
      (* (- x2 x1) (- y2 y1))
      )
  )

(defn contains-point? [rectangle point]
   (let  [ [[x1 y1]  [x2 y2]] rectangle
            [px py]           point   ]
     (and (<= x1 px x2) (<= y1 py y2))
      )
  )

(defn contains-rectangle? [outer inner]
   (let  [ [[x1 y1]  [x2 y2]] outer
           [[i1 j1]  [i2 j2]] inner
          ]
         (and (contains-point? outer [i1 j1]) (contains-point? outer [i2 j2]))
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
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)
        ]
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
  (let [second-element (fn [vec] (get vec 1))]
    (map second-element collection)
    )
   )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (let [ ismonoup (apply <= (vec a-seq))
         ismonodown   (apply >= (vec a-seq))
         ]
    (or ismonoup ismonodown)
    )
  )

(defn stars [n]
  (apply str (repeat n "*") )
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
  (let [ author-names (fn [book] (:authors book))]
      (apply clojure.set/union (map author-names books))
         )
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [author-name (:name author)
        author-dod  (:death-year author)
        author-dob  (:birth-year author)
        author-years (str "(" author-dob " - " author-dod ")")]
      (if author-dob (str author-name " " author-years) (str author-name))
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [book-title (:title book)
        author-list (authors->string (:authors book))]
    (str book-title ", written by " author-list)
    )
  )

(defn books->string [books]
  (let [nr-of-books (count books)]
    (cond
      (== nr-of-books 0)   (str "No books.")
      (== nr-of-books 1)   (apply str (vector "1 book. " (apply str (map book->string books)) "."))
      :else                (apply str (vector nr-of-books " books. " (apply str (interpose ". " (map book->string books))) "."))
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books )
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
  )

; %________%
