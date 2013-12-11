(ns structured-data)

(defn do-a-thing [x]
  (let [z (+ x x)]
    (Math/pow z z)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x1 x2 x3]]
  (+ x1 x3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[[x1 y1] [x2 y2]] [px py]] [rectangle point]]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  ;(assoc book :authors (conj (book :authors) new-author)))
  (let [original (conj (book :authors) new-author)]
    (assoc book :authors original)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn author-names [book]
  (map :name (:authors book)))

(defn element-lengths [collection]
  (let [calculate_length (fn [x] (count x))]
    (map calculate_length collection)))

(defn second-elements [collection]
  (let [get_second (fn [x] (get x 1))]
    (map get_second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors) )))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  ;apply clojure.set/union [#{1 2} #{5} #{7 8}]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  ;(str (:name author) " ("))
  (let [name (:name author)
        birth_year (:birth-year author)
        death_year (:death-year author)]
    (cond
     (contains? author :death-year) (str name " (" birth_year " - " death_year ")")
     (contains? author :birth-year) (str name " (" birth_year " - )")
     :else (str name))
    ))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [number_of_books (count books)
        books_into_string (apply str (interpose ", "(map book->string books)))]
    (cond
     (< number_of_books 1) "No books."
     (== 1 number_of_books) (str number_of_books " book. " books_into_string ".")
     (< 1 number_of_books) (str number_of_books " books. " books_into_string ".")
     )
    )
  )

(defn books-by-author [author books]
  ; has-author? [book author]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (let [author (filter
               (fn [person] (= (:name person) name))
               authors)
        ]
    (cond
     (empty? author) nil
     :else (first author))
    )
  )

(defn living-authors [authors]
  (filter
   ; alive? [author]
   (fn [author] (alive? author))
   authors)
  )

(defn has-a-living-author? [book]
  (let [alive (living-authors (:authors book))
        ]
    (not (empty? alive)))
  )

(defn books-by-living-authors [books]
  (filter
   (fn [book] (has-a-living-author? book))
   books)
  )

; %________%
