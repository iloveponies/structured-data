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
  [x y]
  )

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (=
      (height rectangle)
      (width rectangle)
      )
    )
  )

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (*
      (height rectangle)
      (width rectangle)
      )
    )
  )

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and
    (<= x1 x x2)
    (<= y1 y y2)
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and
      (contains-point? outer [x1 y1])
      (contains-point? outer [x2 y2])
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
  (assoc
    book :authors
    (conj
      (:authors book) new-author)
    )
  )

(defn alive? [author]
  (not
    (contains? author :death-year)
    )
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map second collection)
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
    )
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem)
    (disj a-set elem)
    :else
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (let [setti (set a-seq)]
    (cond
      (= (count setti)
         (count a-seq)) false
      :else
      true
      )
    )
  )


(defn old-book->new-book [book]
  (assoc book :authors
    (set
      (:authors book)
      )
    )
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)
         )
  )

(defn all-author-names [books]
  (set
    (map :name (authors books))
    )
  )

(defn author->string [author]
  (let
    [by (:birth-year author)
     dy (:death-year author)
     name (:name author)]
    (if by
      (str name " (" by " - " dy ")")
      (str name)
      )
    )
  )

(defn authors->string [authors]
  (apply str
    (interpose
      ", "
      (map author->string authors)
      )
    )
  )

(defn book->string [book]
  (let [newb (old-book->new-book book)
        title (:title newb)
        authors (:authors newb)
        astr (authors->string authors)]
    (str title ", written by " astr))
    )


(defn books->string [books]
  (let
    [bc (count books)
     bstrings (apply str
                (interpose
                  ". "
                  (map book->string books)))
     ]
    (apply str
       (if (= bc 0)
         (str "No books")
         (cond
           (> bc 1) (str bc " books. " bstrings)
           (= bc 1) (str bc " book. " bstrings)
           )
         )
        "."
       )
    )
  )

(defn books-by-author [author books]
  (filter
    (fn [x]
      (has-author? x author))
    books
    )
  )

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
