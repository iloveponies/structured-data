(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v] 
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1][x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1][x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle 
          [px py] point] 
      (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
    (let [[p1 p2] inner] 
      (and (contains-point? outer p1) (contains-point? outer p2)))) 

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [a (:authors book)
        new-authors (conj a new-author)]
        (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map #(count %) collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

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
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map #(:authors %) books)))

(defn all-author-names [books]
  (let [a (authors books)]
    (set (map #(:name %) a))))


(defn life-span [author] 
  (if (contains? author :birth-year) 
    (if (contains? author :death-year) 
      (str " (" (:birth-year author) " - " (:death-year author) ")")
      (str " (" (:birth-year author) " - )"))
    nil))

(defn author->string [author]
  (str (:name author) (life-span author)))

(defn authors->string [authors]
  (let [author-strings (map author->string authors)]
    (apply str (interpose ", " author-strings))    ))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-strings (map book->string books)
        parsed (apply str (interpose ", " book-strings))
        num (count books)
        label (if (> num 1) " books. " " book. ")]
    (if (> num 0)
      (str num label parsed ".")
      "No books.")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
