(ns structured-data)

(defn do-a-thing [x]
  (let [x-doubled (+ x x)]
    (Math/pow x-doubled x-doubled)
  ))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[llx lly] [urx ury]] rectangle]
    (- urx llx)))

(defn height [rectangle]
  (let [[[llx lly] [urx ury]] rectangle]
    (- ury lly)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[llx lly] [urx ury]] rectangle
        [x y] point]
    (and (<= llx x urx) (<= lly y ury))))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (get inner 0))
       (contains-point? outer (get inner 1))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [s] (get s 1))]
    (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (reduce clojure.set/union (map :authors books)))


;; not sure this is idiomatic...
(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        born (:birth-year author)
        died (:death-year author)]
    (cond
     died (str name " (" born " - " died ")")
     born (str name " (" born " - )")
     :else name)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [n (count books)
        entries (apply str (interpose ". " (map book->string books)))]
    (cond
     (= n 0) (str "No books.")
     (> n 1) (str n " books. " entries ".")
     :else   (str n " book. "  entries "."))))

(defn books-by-author [author books]
  (let [pred (fn [b] (has-author? b author))]
    (filter pred books)))

(defn author-by-name [name authors]
  (let [pred (fn [author] (= (:name author) name))]
    (first (filter pred authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
