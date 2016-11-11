(ns structured-data)

(defn do-a-thing [x]
  (let [xdouble (+ x x)]
    (Math/pow xdouble xdouble)))

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

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs (- x2 x1)))

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y2 y1)))

(defn square? [[[x1 y1] [x2 y2]]]
  (let [h (Math/abs (- y2 y1))
        w (Math/abs (- x2 x1))]
  (if (= h w) true false )
  ))

(defn area [[[x1 y1] [x2 y2]]]
  (let [h (Math/abs (- y2 y1))
        w (Math/abs (- x2 x1))]
  (* h w)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
  (if (and (or (<= x1 x3 x2) (>= x1 x3 x2))
      (or (<= y1 y3 y2) (>= y1 y3 y2)))
    true
    false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and (contains-point? outer [x3 y3])
         (contains-point? outer [x4 y4])
         )))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get2 (fn [x] (get x 1))]
    (map get2 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*" )))

(defn toggle [a-set elem]
  (if (= (contains? a-set elem) true)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (let [setted (set a-seq)]
    (if (< (count setted) (count a-seq))
      true
      false)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (:authors book)) books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author)
       (if (contains? author :birth-year)
         (str " (" (:birth-year author) " - " (:death-year author)")"))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string(:authors book))))

(defn books->string [books]
    (cond
      (= (count (map book->string books)) 0) "No books."
      (= (count (map book->string books)) 1) (str "1 book. " (apply book->string books) ".")
      (> (count (map book->string books)) 1) (str (count (map book->string books)) " books. " (apply str (interpose " " (map #(str % ".") (map book->string books)))))
      ))


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
