(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (let [[[x1 x2][y1 y2]] rectangle]
  (- y1 x1)
  ))

(defn height [rectangle]
  (let [[[x1 x2][y1 y2]] rectangle]
  (- y2 x2)
  ))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1 x2][y1 y2]] [z1 z2]]
    (and (<= x1 z1 y1) (<= x2 z2 y2) )
)
(defn contains-rectangle? [outer [bottom_left top_right]]
  (and (contains-point? outer bottom_left) (contains-point? outer top_right))
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
   (count (:authors book)))

(defn multiple-authors? [book]
   (>(count (:authors book)) 1)
)

(defn add-author [book new-author]
  (let [original_authors (:authors book)
        new_authors (conj original_authors new-author)]
    (assoc book :authors new_authors)))

(defn alive? [author]
  (if(contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second_elements (fn [i] (get i 1))]
    (map second_elements collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n \*))
  )


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem) )
  )

(defn contains-duplicates? [a-seq]
  (let [copy (set a-seq)]
    (< (count copy) (count a-seq)))
  )

(defn old-book->new-book [book]
  (let [old_authors (:authors book)
        new_authors (set old_authors)]
  (assoc book :authors new_authors))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
 (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name-part (:name author)
       year-part (str " (" (:birth-year author) " - " (:death-year author) ")" )]
    (if (contains? author :birth-year)
      (str name-part year-part)
      name-part)

    )
  )

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [book_count (count books)
        book_info (fn [book] (book->string book))]
    (cond
     (= book_count 0) "No books."
     (= book_count 1) (str "1 book. " (apply book_info books) ".")
     :else (str book_count " books. " (apply str (interpose ". " (map book_info books))) ".")
     ))
  )

(defn books-by-author [author books]
  (filter (fn [books] (has-author? books author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book) ) ))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
