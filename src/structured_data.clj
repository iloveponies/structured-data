(ns structured-data)

(defn do-a-thing [x]
 (let [x2 (+ x x)]
   (Math/pow x2 x2))
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c))
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2)))
)

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2)))
)

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
)

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
)

(defn contains-point? [rectangle point]
  (let [ [x3 y3] point
         [[x1 y1] [x2 y2]] rectangle ]
    (and
      (<= x1 x3 x2)
      (<= y1 y3 y2)))
)

(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2)))
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
  (let [authors (:authors book)
        updated (conj authors new-author)
        ]
    (assoc book :authors updated)
    )
)

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [get2 (fn [x] (get x 1))]
    (map get2 collection)
    )
)

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (let [new-book (old-book->new-book book)]
    (contains? (:authors new-book) author))
)

(defn authors [books]
  (apply clojure.set/union (map :authors books))
)

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))
    )
)

(defn author->string [author]
  (let [auth-name (:name author)]
    (if (contains? author :birth-year)
      (str auth-name " " (str "(" (:birth-year author) " - " (:death-year author "")) ")")
      auth-name
    )
    )
)

(defn authors->string [authors]
  (let [auth-strs (map author->string authors)]
    (apply str (interpose ", " auth-strs))
    )
)

(defn book->string [book]
  (let [author-strs (authors->string (:authors book))
        title (:title book)]
    (str title ", written by " author-strs)
    )
)

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [bcount (count books)
          bcount-str (str bcount " " (if (> bcount 1) "books. " "book. "))
          book-strs (map book->string books)]
      (str bcount-str (apply str (interpose ", " book-strs)) ".")
      )
    )
)

(defn books-by-author [author books]
  (let [takeit? (fn [book] (has-author? book author))]
    (filter takeit? books))
)

(defn author-by-name [name authors]
  (let [takeit? (fn [author] (= name (:name author)))
        filtered (filter takeit? authors)]
    (first filtered))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (let [alive-authors (living-authors (:authors book))]
    (not (empty? alive-authors))
    )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
