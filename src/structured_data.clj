(ns structured-data)

(defn do-a-thing [x]
  (let [plus (+ x x)]
    (Math/pow plus plus)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

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
  (let [[[x1] [x2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x2 x1) (- y2 y1))
      true
      false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
    ))

(defn contains-rectangle? [outer inner]
  (let [[bottom_left top_right] inner]
    (and (contains-point? outer bottom_left) (contains-point? outer top_right))
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1)
  )

(defn add-author [book new-author]
  (let [original_authors (:authors book)
        new_authors (conj original_authors new-author)]
    (assoc book :authors new_authors)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second_elements (fn [x] (get x 1))]
    (map second_elements collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [copy (set a-seq)]
    (< (count copy) (count a-seq))))

(defn old-book->new-book [book]
  (let [old_authors (:authors book)
        new_authors (set old_authors)]
  (assoc book :authors new_authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author_name (:name author)
        author_years (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year)
      (str author_name author_years)
      author_name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book_count (count books)
        book_details (fn [book] (book->string book))]
    (cond
     (= book_count 0) "No books."
     (= book_count 1) (str "1 book. " (apply book_details books) ".")
     :else (str book_count " books. " (apply str (interpose ". " (map book_details books))) ".")
     )))

(defn books-by-author [author books]
  (filter (fn [books] (has-author? books author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)
  ))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%





