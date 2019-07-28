(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (>= (count v) 3)
  (+ (get v 0) (get v 2))
    nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (>= (count v) 3)
    (let [[a b c] v]
     (+ a c))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))))

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
  (if (== w h) true false)))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (if (and (<= x1 x x2)
             (<= y1 y y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
        (if (and (contains-point? outer p1)
                 (contains-point? outer p2))
          true
          false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new_authors (conj authors new-author)]
    (assoc book :authors new_authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [se (fn [x] (get x 1))]
    (map se collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (boolean (some true? [(apply <= a-seq) (apply >= a-seq)])))

(defn stars [n]
  (apply str (take n (repeat "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [cnt (count a-seq)
        set-cnt (count (set a-seq))]
    (not= cnt set-cnt)))

(defn old-book->new-book [book]
  (let [authors (:authors book)
        author-set (set authors)]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (let [get-authors (fn [x] (:authors x))]
    (apply clojure.set/union (map get-authors books))))

(defn all-author-names [books]
  (let [name (fn [author] (:name author))]
    (set (map name (authors books)))))

(defn author->string [author]
  (let [name (:name author)
        by (:birth-year author)
        dy (:death-year author)]
    (if (or by dy)
      (str name " (" by " - " dy ")")
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [cnt (count books)
        noun (if (> cnt 1) "books" "book")
        books (apply str (interpose ". " (map book->string books)))]
    (if (== cnt 0)
      "No books."
      (str cnt " " noun ". " books "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
