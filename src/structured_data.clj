(ns structured-data)

(defn do-a-thing [x]
  (let [two-of-these (+ x x)]
    (Math/pow two-of-these two-of-these))
)

(defn spiff [v]
  (if(and (> (count v)) (number? (get v 0)) (number? (get v 2)) 2)
      (+ (get v 0) (get v 2))
      "nope"
  )
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (if (> (count v) 2)
    (let [[a b c] v]
      (if (and (number? a) (number? c))
        (+ a c)
        "nope"
      )
    )
    "nope"
  )

)

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
    (- y2 y1)
  )
)

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
      true
      false
    )
  )
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
  )
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[px py] point]
      (if (and (<= x1 px x2) (<= y1 py y2))
        true
        false
      )
    )
  )
)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))
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
  (if (> (count (:authors book)) 1)
    true
    false
  )
)

(defn add-author [book new-author]
  (let [author-array (:authors book)]
    (assoc book :authors (conj author-array new-author))
  )
)

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true
  )
)

(defn element-lengths [collection]
  (map
    (fn [collection-element]
      (count collection-element))
   collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  (apply
    str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq)))
    true
    false))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (if (contains? (book :authors) author)
    true
    false))

(defn authors [books]
  (let [each-author (fn [book] (:authors book))]
    (apply clojure.set/union (map each-author books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        author-birth (:birth-year author)
        author-death (:death-year author)]
   (cond
      (not (alive? author)) (str author-name " (" author-birth " - " author-death ")")
      (not (nil? author-birth)) (str author-name " (" author-birth " - )")
      :else author-name)))

(defn authors->string [authors]
  (let [each-author (map author->string authors)]
    (apply str (interpose ", " each-author))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [each-book (map book->string books)
                num-of-books 
                  (if (= (count books) 1)
                    "1 book. "
                    (str (count books) " books. "))]
            (str (apply str num-of-books (interpose ". " each-book)) \.))))

(defn books-by-author [author books]
  (filter (fn [single-book] (has-author? single-book author)) books))

(defn author-by-name [name authors]
  (let [author-found (first (filter (fn [single-author] (= (:name single-author) name)) authors))]
    (if (empty? author-found)
      nil
      author-found)))

(defn living-authors [authors]
  (filter (fn [single-author] (alive? single-author)) authors))

(defn has-a-living-author? [book]
  (let [some-alive (filter (fn [single-author] (alive? single-author)) (:authors book))]
    (if (not (empty? some-alive))
      true
      false)))

(defn books-by-living-authors [books]
  (filter (fn [one-book] (has-a-living-author? one-book)) books))

; %________%
