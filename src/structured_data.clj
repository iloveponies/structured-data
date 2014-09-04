(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)
    ))

(defn spiff [v]
  (if (and (vector? v) (>= (count v) 3))
    (+ (get v 0) (get v 2))
   ))

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (if (and (vector? v) (>= (count v) 3))
    (let [[x y z] v]
      (+ x z)
      )
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs (- x1 x2)))  

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y1 y2)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (<= x1 px x2) (<= y1 py y2)))

(defn contains-rectangle? [outer [inner-bottom-corner inner-top-corner]]
  (and (contains-point? outer inner-bottom-corner)
       (contains-point? outer inner-top-corner)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)]
    (assoc book :authors (conj old-authors new-author))))

(defn alive? [author]
  (= (:death-year author) nil))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem) ;remove if contains
    (conj a-set elem) ;add if does not
    ))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
        ]

    (if birth
      (str name " (" birth " - " death ")") ;at least birth year known
      (str name) ;no years known
      )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [books-list-string (fn [x] (apply str (interpose ". " (map book->string books))))]

    (cond 
     (empty? books) "No books."
     (= (count books) 1) (str "1 book. " (book->string (get books 0)) ".")
     (> (count books) 1) (str (count books) " books. " (books-list-string books) ".")
     )))

(defn books-by-author [author books]
  (let [has-this-author? (fn [b] (has-author? b author))]
    (filter has-this-author? books)
  ))

(defn author-by-name [name authors]
  (let [has-this-name? (fn [a] (= name (:name a)))]
    (first (filter has-this-name? authors))
  ))

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
