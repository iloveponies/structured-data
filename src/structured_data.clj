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
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bx by] [tx ty]]]
  (- tx bx))

(defn height [[[bx by] [tx ty]]]
  (- ty by))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[bx by] [tx ty]] [px py]]
  (and (<= bx px tx) (<= by py ty)))

(defn contains-rectangle? [outer [bottom-left top-right]]
  (and (contains-point? outer bottom-left) (contains-point? outer top-right)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [current-authors (:authors book)
        updated-authors (conj current-authors new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [pair] (get pair 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
          (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [current-authors (:authors book)]
    (if (nil? current-authors)
      book
      (assoc book :authors (set current-authors))))) 

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
      (not (nil? death-year)) (str name " (" birth-year " - " death-year ")")
      (not (nil? birth-year)) (str name " (" birth-year " - )")
      :else name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [booksstr (apply str (interpose ". " (map book->string books)))
        bookcount (count books)]
    (cond
      (= bookcount 0) "No books."
      (= bookcount 1) (str "1 book. " booksstr ".")
      :else (str bookcount " books. " booksstr "."))))

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
