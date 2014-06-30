(ns structured-data)

(defn do-a-thing [x]
  (let [dblx (+ x x)] (Math/pow dblx dblx))
  )

(defn spiff [v]
  (let [first (get v 0)
	third (get v 2)]
	(+ first third)

    )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2] ] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
         [px py] point]
     (and
      (<= x1 px x2)
      (<= y1 py y2))
     ))



 (defn contains-rectangle? [outer inner]
  (let [[lowerleft upperright] inner]
    (and (contains-point? outer lowerleft)
         (contains-point? outer upperright))
    )
   )

 (defn title-length [book]
  (count (:title book)))



(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )


(defn add-author [book new-author]
  (let [orig-authors (:authors book)]
    (assoc book :authors (conj orig-authors new-author))
    )
  )


(defn alive? [author]
  (not (contains? author :death-year))
  )


(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [secondel (fn [x] (get x 1))]
    (map secondel collection)
    )
  )


(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or ( apply <= a-seq)
      ( apply >= a-seq)
             )

)

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )


(defn contains-duplicates? [a-seq]
   (> (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (let [new-authors (set
                     (:authors book))]
    (assoc book :authors new-authors))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (let [auth (fn [book] (:authors book))]
     (set (apply concat (map auth books)
  )))
  )

(defn all-author-names [books]
  (let [authorsnames (authors books)]
     (set (map :name authorsnames)))

  )


(defn author->string [author]
  (let [name (:name author) birth (:birth-year author) death (:death-year author)]
    (str name (if (contains? author :birth-year) (str " (" birth " - " death ")")))
  )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [title (:title book) authors (:authors book)]
    (str title ", written by " (authors->string authors))
    )
  )

(defn books->string [books]
  (let [numbooks (count books)
        titles (apply str (interpose ". " (map book->string books))
        )]
    (cond (= 0 numbooks) "No books."
          (= 1 numbooks)  (str "1 book. " titles ".")
          :else (str numbooks " books. " titles ".")
          )

    )

  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author?  book author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))

  )

(defn living-authors [authors]

  (filter alive? authors)

  )



(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )



(defn books-by-living-authors [books]

  (filter  has-a-living-author? books)
  )


; %________%
