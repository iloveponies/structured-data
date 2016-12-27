(ns structured-data)

(defn do-a-thing [x]
  (let [x-sum (+ x x)]
    (Math/pow x-sum x-sum)))

(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn cutify [v]
  (cond
    (vector? v) (conj v "<3")
    :else (throw ex-info {:cause "Input not a vector"})))

(defn spiff-destructuring [v]
  (let [[a _ b & rest] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x-1 _] [x-2 _]] rectangle]
    (- x-2 x-1)))

(defn height [rectangle]
  (let [[[_ y-1] [_ y-2]] rectangle]
    (- y-2 y-1)))

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (= w h)))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[leftmost-x bottom-y]
         [rightmost-x top-y]] rectangle
        [argument-x argument-y] point]
    (and (<= leftmost-x argument-x rightmost-x)
         (<= bottom-y argument-y top-y))))

(defn contains-rectangle? [outer inner]
  (let [[argument-point-1 argument-point-2] inner]
    (and (contains-point? outer argument-point-1)
         (contains-point? outer argument-point-2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second
          (fn [v]
            (let [[_ second-element & _] v]
              second-element))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (take n (repeat "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)
        new-authors (set authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        year-of-birth (:birth-year author)
        year-of-death (:death-year author)]
    (cond
      (and year-of-birth year-of-death)
        (str author-name " (" year-of-birth " - " year-of-death ")")
      (and year-of-birth)
        (str author-name " (" year-of-birth " - )")
      :else
        author-name)))

(defn authors->string [authors]
  (->> authors
       (map author->string)
       (clojure.string/join ", ")))

(defn book->string [book]
  (let [book-title (:title book)
        authors-as-string (authors->string (:authors book))]
    (str book-title ", written by " authors-as-string)))

(defn books->string [books]
  (cond
    (empty? books)
      "No books."
    (= (count books) 1)
      (str "1 book. " (book->string (first books)) ".")
    (> (count books) 1)
      (let [book-strings-formatted
              (clojure.string/join " "
                                   (map #(apply str % ".")
                                   (map book->string books)))]
        (str "3 books. " book-strings-formatted))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (> (count (filter #(alive? %) (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
