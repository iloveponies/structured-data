(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (height rectangle) (width rectangle))
    ))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (height rectangle) (width rectangle))
  ))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
          [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))
  ))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2))
  ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
 (map count collection))

(defn second-elements [collection]
  (let [take2nd (fn [c] (get c 1))]
    (map take2nd collection)))

(defn titles [books]
  (map (fn [b] (:title b)) books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors
    (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map (fn [b] (:authors b))
         books)))

(defn all-author-names [books]
  (set (map (fn [a] (:name a)) (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str "(" (:birth-year author) " - " (:death-year author) ")")]
   (if (nil? (:birth-year author))
     name
     (str name " " years))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

 (defn books->string [books]
  (let [book_count (count books)]
   (apply str
   (if (== 0 book_count) "No books."
     (if (== 1 (count books))
       "1 book. "
       (str book_count " books. ")))
    (interpose " "(map (fn [b] (str (book->string b) ".")) books)))))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (:name a))) authors)))

(defn living-authors [authors]
  (filter (fn [a] (alive? a)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
