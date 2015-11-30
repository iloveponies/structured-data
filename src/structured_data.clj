(ns structured-data)

(defn do-a-thing [x]
  (let [add (+ x x)]
    (Math/pow add add)))

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
  (let [[bottom-left top-right] rectangle]
    (- (first top-right) (first bottom-left))))

(defn height [rectangle]
  (let [[[bx by] [tx ty]] rectangle]
    (- ty by)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[bx by] [tx ty]] rectangle
        [x y] point]
    (and (and (<= x tx) (>= x bx))
         (and (<= y ty) (>= y by)))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (>= (author-count book) 2)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors
              (conj (book :authors) new-author)))

(defn alive? [author]
  (if (:death-year author)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

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
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (str (:name author)
       (if (:birth-year author)
         (str " ("
              (:birth-year author)
              " - "
              (:death-year author)
              ")")
         "")))

(defn authors->string [authors]
  (apply str
         (interpose ", " (map author->string authors))))

(defn book->string [book]
  (cond (empty? book) ""
        :else
        (str
          (:title book)
          ", written by "
          (authors->string (:authors book)))))

(defn books->string [books]
  (cond (empty? books) "No books."
        :else
        (str
          (count books)
          (if (> (count books) 1)
            " books. "
            " book. ")
          (apply str
                 (interpose ". "
                            (map book->string books)))
          "."
          )))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (let [author (filter #(not (= nil %))
                       (map #(if (= name (:name %)) %)
                            authors))]
    (if (empty? author)
      nil
      (first author))))


(defn living-authors [authors]
  (filter #(not (= nil %)) (map #(if (alive? %)
                                  %) authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(not (= nil %))
          (map #(if (has-a-living-author? %) %) books)))

; %________%
