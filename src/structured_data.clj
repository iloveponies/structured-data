(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

(defn spiff [v]
  (if (> 3 (count v))
    nil
    (let [third (get v 2)]
      (+ (first v) third))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (> 3 (count v))
    nil
    (let [[x y z] v]
      (spiff [x y z]))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
(if
  (== (height rectangle) (width rectangle))
  true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2))
      true false)
    ))

(defn contains-rectangle? [outer inner]
  (let [[[a1 b1] [a2 b2]] inner]
    (if (and (contains-point? outer (point a1 b1))
             (contains-point? outer (point a2 b2)))
      true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
  true false))

(defn stars [n]
  (str (apply str (repeat n "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (<
       (count (set a-seq))
       (count a-seq))
    true false))

(defn old-book->new-book [book]
  (let [all_authors (:authors book)]
   (assoc book
    :authors
    (set all_authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
(let [author-names
      (fn [book] (map :name (:authors book)))]
  (set (apply clojure.set/union
              (map author-names books)))))


(defn author->string [author]
  (let [name (:name author)
        years [(:birth-year author) (:death-year author)]
        str-years (if-let [years (= (first years) nil)]
                    ""
                    (str " (" (first years) " - " (second years) ")"))]
    (str name str-years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (set (:authors book)))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [num (count books)
        str-num (cond (= num 0)
                      "No books"
                      (= num 1)
                      "1 book. "
                      :else (str num " books. "))
        book-list (apply str (interpose ". " (map book->string books)))]
    (str str-num book-list ".")))

(defn books-by-author [author books]
  (filter
   (fn[x] (has-author? x author))
   books))

(defn author-by-name [name authors]
  (let [result (filter (fn[x] (= name (:name x))) authors)]
    (if (empty? result)
      nil
      (first result))))

(defn living-authors [authors]
  (filter (fn[x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn[x] (has-a-living-author? x)) books))

; %________%
