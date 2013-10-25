(ns structured-data)

(defn do-a-thing [x]
  (let [add (+ x x)]
    (Math/pow add add)
    )
  )

(defn spiff [v]
  (+(get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
  (+ a c)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  ))

(defn height  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
  ))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (- y2 y1) (- x2 x1))
      true
      false)
  ))

(defn area [rectangle]
    (*(height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[xPoint yPoint] point]
    (if (and (<= x1 xPoint x2)  (<= y1 yPoint y2))
      true
      false)
    ))
  )

(defn contains-rectangle? [outer inner]
  (let [[xPoint yPoint] inner]
    (if (and (contains-point? rectangle xPoint) (contains-point? rectangle yPoint))
      true
      false
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
  (if(< 1 (author-count book) )
     true
     false)
  )

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (conj authors new-author)
    (assoc book :authors authors))
  )

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [c] (get c 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (apply <= a-seq))

(defn stars [n]
  (apply str(apply concat(repeat n "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq)))
    true
    false)
  )

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
  (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names (fn [book] (map :name (:authors book)))]
    map author-names books))

(defn all-author-names [books]
  :-)

(defn author->string [author]
  (let [name (fn [author] (:name author))]
    (let [year (fn [author](str " (" (:birth-year author) " - " (:death-year author)")"))]
      (if (nil?(:birth-year author))
        (str (name author))
        (str (name author) (year author))
      )))
  )

(defn authors->string [authors]
  (apply str(interpose ", "(apply author->string authors)))
  )

(defn book->string [book]
  (str (:title book)))

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first(filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [author] alive? author) authors)
  )

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
