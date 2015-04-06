(ns structured-data)


(defn do-a-thing [x]
  (let [a (+ x x)]
  (Math/pow a a)))

(defn spiff [v]
  ( + (v 0) (v 2)))

(defn cutify [v]
  ( conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
        (and (<= x1 px x2) (<= y1 py y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
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
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [c](get c 1))]
        (map second collection)
  )
  )

(defn titles [books]
  (map :title books)
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
  (not (== (count (set a-seq)) (count a-seq)))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (book :authors) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (:name author)
         birth (:birth-year author)
         death (:death-year author)]
    (cond
     (contains? author :death-year) (str name " (" birth " - " death ")")
     (contains? author :birth-year) (str name " (" birth " - " ")")
     :else name)
    )

   )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )


(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
   (str title ", written by " (authors->string authors)))
  )

(defn books->string [books]
  (let [count (count books)
        books (apply str (interpose ", " (map book->string books)))]
    (cond
     (== count 0) "No books."
     (== count 1) (str "1 book. " books ".")
     :else (str count " books. " books ".")))
  )

(defn books-by-author [author books]
  (filter (fn[x] (has-author?  x author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
   (> (count(living-authors (:authors book))) 0)
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
