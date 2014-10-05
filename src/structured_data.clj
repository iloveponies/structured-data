(ns structured-data)

(defn do-a-thing [x]
  (let [addem (+ x x)]
    (Math/pow addem addem)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2) )
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
  (let [[p1 p2] rectangle]
    (- (get p2 0) (get p1 0)))
  )

(defn height [rectangle]
  (let [[p1 p2] rectangle]
    (- (get p2 1) (get p1 1)))
  )

(defn square? [rectangle]
  (== (height rectangle) (width rectangle) )
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[p1 p2] rectangle
        [x y] point]
    (if (and (<= (get p1 0) x (get p2 0))
          (<= (get p1 1) y (get p2 1)) )
      true false)
    )
  )

(defn test? [x y]
  false)

(defn contains-rectangle? [outer inner]
  (let [[inner_low inner_high] inner]
    (if (and (contains-point? outer inner_low)
          (contains-point? outer inner_high))
      true false
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
  (if (> (author-count book) 1)
    true false)
  )

;(defn contains-author? [ author-list author i ]
;  (if (= (get author-list i) nil) false
;    (if (= (:name (get author-list i)) author)
;      true
;      (contains-author? author-list author (+ i 1))
;      )
;    )
;  )

(defn add-author [book new-author]
  (let [authors (:authors book)
        name (:name new-author)
        contains-author? (fn contains-author? [author-list author i]
                           (if (= (get author-list i) nil) false
                             (if (= (:name (get author-list i)) author)
                               true
                               (contains-author? author-list author (+ i 1))
                               )
                             )
                           )
        ]
    (if (contains-author? authors name 0)
      false
      (let [new-authors (conj authors new-author)]
        (assoc book :authors new-authors))
      )
    )
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (let [length (fn [x]
                 (count x))]
    (map length collection)
    )
  )

(defn second-elements [collection]
  (let [get-second (fn [x]
                     (get x 1))]
    (map get-second collection))
  )

(defn titles [books]
    (map (fn [x] (:title x)) books)
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
  (not= (count (set a-seq) ) (count a-seq) )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
    (contains? (:authors (old-book->new-book book)) author)
  )

(defn authors [books]
 (set (apply clojure.set/union (map :authors books))) 
)

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [ name (:name author)
        years (if (:birth-year author)
                (:birth-year author)
                nil
                )]
    (apply str (concat [name 
                        (if years 
                          (str " (" years " - " 
                              (if (:death-year author) 
                                (str (:death-year author) ")" ) ")"
                                )
                              )
                          )]))
  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors )))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book) ))
  )

(defn books->string [books]
  (let [c (count books)
        my-books (str (apply str (interpose ". " (map (fn [x] (str (book->string x))) books ) )  ) ".")  ]
    (if (not= c 0) 
      (if (== c 1)
        (str c " book. " my-books)
        (str c " books. " my-books)) 
      (str "No books.")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author) ) books )
  )

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x))) authors )
  ))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))
  ))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books) 
  )

; %________%
