(ns structured-data)

(defn do-a-thing [x]
 (let [plus (+ x x) ]
   (Math/pow plus plus)
 )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)))

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
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2 ) (<= y1 p2 y2))
  ))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and
     (contains-point? outer point1)
     (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
 (let [au (:authors book)]
   (conj au new-author)
   (assoc book :authors (conj au new-author))
   )
)

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]

  (let [seccy (fn [x] (get x 1) ) ]
    (map seccy collection)
    )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or
   (apply >= a-seq)
   (apply <= a-seq))
  )


(defn stars [n]
  (apply str (repeat n "*"))

  )

(defn toggle [a-set elem]

  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  ))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq)
      (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [au (set (:authors book))]
  (assoc book :authors au)
  ))

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set ( map :name (authors books)))
  )

(defn author->string [author]
  (let [vuosi (if (contains? author :birth-year)
                (str " (" (:birth-year author) " - " (:death-year author) ")")
                nil)]
    (str (:name author) vuosi)))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))

  )

(defn book->string [book]
  (let [writer (if (contains? book :authors)
                      (str ", written by " (authors->string (:authors book)))
                      nil)]
    (str (:title book) writer)))

(defn books->string [books]
  (let [numb (count books)
        numb-str (cond
                  (== numb 0) "No books."
                  (== numb 1) "1 book."
                  :else (str numb " books."))
        descs (map book->string books)
        add-points (interpose ". " descs)
        res-str (apply str add-points)
        res-str-fin (if (> (count books) 0) (str " " res-str ".") res-str)]
    (str numb-str res-str-fin)))

(defn books-by-author [author books]
  (let [has-written? (fn [x] (has-author? x author))]
        (filter has-written? books)))

(defn author-by-name [name authors]
  (let [is-called? (fn [x] (if (= (:name x) name) true false))]
        (first (filter is-called? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%








