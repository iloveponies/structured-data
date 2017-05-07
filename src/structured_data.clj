(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [[ [x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (width rectangle)
      (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and
     (<= x1 px x2)
     (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and
     (contains-point? outer [x1 y1])
     (contains-point? outer [x1 y2])
     (contains-point? outer [x2 y1])
     (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [old_authors (:authors book)
        new_authors (conj old_authors new-author)]
    (assoc book :authors new_authors)))

(defn alive? [author]
   (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply <= (reverse a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not
   (== (count a-seq)
       (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors_vec (:authors book)
        authors_set (set authors_vec)]
    (assoc book :authors authors_set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-y (:birth-year author)
        death-y (:death-year author)]
    (cond 
     (nil? birth-y) name
     (nil? death-y) (str name " (" birth-y " - )")
     :else (str name " (" birth-y " - " death-y ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))

(defn books->string [books]
  (let [cnt (count books)
        books_list (map book->string books)
        books_inter (interpose ". " books_list)
        books_str (apply str books_inter)]
    (cond
     (== cnt 0) "No books."
     (== cnt 1) (str "1 book. " books_str ".")
     :else       (str cnt " books. " books_str)))
  )

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (let [matching_authors (filter #(= (:name %) name) authors)
        cnt (count matching_authors)]
    (if (< 0 cnt) (first matching_authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [living-book-authors (living-authors (:authors book))]
    (not (== (count living-book-authors) 0))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
