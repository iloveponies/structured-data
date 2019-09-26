(ns structured-data)

(defn do-a-thing [x]
  (let [twice-x (+ x x)]
    (Math/pow twice-x twice-x)))

(defn spiff [v]
  (let [e1 (get v 0)
        e3 (get v 2)]
    (+ e1 e3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[e1 _ e3] v]
    (+ e1 e3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> x2 x1) 
      (- x2 x1)
      (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y2 y1) 
      (- y2 y1)
      (- y1 y2))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [px py] point]
         (if (and (>= x2 x1) (>= y2 y1))
            (and (<= x1 px x2) (<= y1 py y2))
            (and (<= x2 px x1) (<= y2 py y1)))))

(defn contains-rectangle? [outer inner]
  (let [[ibl itr] inner]
    (and (contains-point? outer ibl) (contains-point? outer itr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secnd (fn [v] (get v 1))]
    (map secnd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name        (:name author)
        birth-year  (:birth-year author)
        death-year  (:death-year author)
        left-paren  (if birth-year " (" "")
        dash        (if birth-year " - " "")
        right-paren (if birth-year ")" "")
        by          (or birth-year "")
        dy          (or death-year "")]
        (str name left-paren by dash dy right-paren)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose ", written by " [(:title book) (authors->string (:authors book))])))

(defn books->string [books]
  (let [num-books (count books)
        book-str (if (> num-books 1) " books. " " book. ")]
    (cond 
      (empty? books) "No books."
      :else (str num-books book-str (apply str (interpose ". " (map book->string books))) ".") 
      )
    ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
