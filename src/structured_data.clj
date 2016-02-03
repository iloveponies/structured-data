(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [first-num (first v)
        third-num (nth v 2)]
    (+ first-num third-num)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[n1 _ n3]]
  (+ n1 n3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]]
  (- x2 x1))

(defn height [[[_ y1] [_ y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (>= (author-count book) 2))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [e] (second e)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (str (:name author))
        has-dob? (contains? author :birth-year)
        has-dod? (contains? author :death-year)
        post (cond
               has-dod? (str " (" (:birth-year author) " - " (:death-year author)  ")")
               has-dob? (str " (" (:birth-year author) " - )")
               :else "")]
    (str name post)))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (let [name (:title book)
        authors (authors->string (:authors book))]
    (str name ", written by " authors)))

(defn books->string [books]
  (let [book-count (count books)
        book-start (cond
                     (>= book-count 2) (str book-count " books. ")
                     (= book-count 1) "1 book. "
                     :else "No books")
        book-titles (clojure.string/join ", " (map book->string books))]
    (str book-start book-titles ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
