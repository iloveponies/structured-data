(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

(defn spiff [v]
  (if (>= (count v) 3)
    (let [x (get v 0)
          y (get v 2)]
        (+ x y))
    false))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn txts [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1) (- y2 y1))))
(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if(and (<= x1 xp x2) (<= y1 yp y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let[[[x1 y1] [x2 y2]] inner
       p1 (point x1 y1)
       p2 (point x2 y2)]
    (if (and (contains-point? outer p1) (contains-point? outer p2))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
      true
      false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secitm (fn [x] (get x 1))]
    (seq (map secitm collection))))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
    :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [count1 (count a-seq) count2 (count (set a-seq))]
    (not(== count1 count2))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
    (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        year (cond (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
                   (contains? author :birth-year) (str " (" (:birth-year author) " - " ")")
                  :else (str "")
                )]
    (str name year)
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [y (cond
           (== (count books) 1) (str "1 book. ")
           (< 1 (count books) ) (str (count books) " books. ")
           :else false
           )]
    (if(= y false)
      (str "No books.")
      (str y (str
              (apply str
                     (interpose ". "
                                (map book->string books)))) "." ))))

(defn books-by-author [author books]
       (filter #(has-author? % author)
               books))

(defn author-by-name [name authors]
  (first
     (filter #(= (:name %) name)
            authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
