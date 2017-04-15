(ns structured-data)

(defn
  do-a-thing
  [x]
  (let [a (+ x x)]
  (Math/pow a a)))

(defn
  spiff
  [v]
  (let [l (count v)
        a (get v 0)
        b (get v 2)]
    (if (> l 2)
      (+ a b)
      "?")
  ))

(defn
  cutify
  [v]
  (conj v "<3")
  )

(defn
  spiff-destructuring
  [v]
  (let [l (count v)]
    (if (> l 2)
      (let [[a x b] v
            ab (+ a b)]
        ab)
      "?"))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (let [a (width rectangle)
        b (height rectangle)]
    (if (= a b)
      true
      false)))

(defn
  area
  [rectangle]
  (let [a (width rectangle)
       b (height rectangle)]
  (* a b))
  )

(defn contains-point? [rectangle point]
  (let [[a b] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 a x2) (<= y1 b y2))))

(defn contains-rectangle? [outer inner]
  (let [[a b] inner]
    (and (contains-point? outer a) (contains-point? outer b)))
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false)
  )

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconder (fn [x] (get x 1))]
    (map seconder collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if
    (<
     (count
      (set a-seq))
     (count a-seq)
     )
    true
    false
    )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books)))
  )
(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [nimi (:name author)
        syntynyt (:birth-year author)
        kuollut (let [pvm (:death-year author)]
                 (if (= pvm nil)
                   ""
                   pvm))
        elanyt (if (= syntynyt nil)
                 ""
                 (str " (" syntynyt " - " kuollut ")"))]
    (str nimi elanyt)
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn
  books->string
  [books]
  (if (= (count books) 0)
    "No books."
    (if (= (count books) 1)
      (str (count books) " book. "
             (apply str (interpose ". " (map book->string books))) ".")
      (str (count books) " books. "
           (apply str (interpose ". " (map book->string books))) ".")
    )
   )
  )

(defn
  books-by-author
  [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%





