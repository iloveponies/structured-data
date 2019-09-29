(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff
  "return the sum of the first and third elements.
  raises a NullPointer exception if (count v) < 3"
  [v]
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
  (let [[[x0 y0] [x1 y1]] rectangle]
    (Math/abs (- x1 x0))))

(defn height [rectangle]
  (let [[[x0 y0] [x1 y1]] rectangle]
    (Math/abs (- y1 y0))))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x0 y0] [x1 y1]] rectangle
        [x y] point
        [xa xb] [(Math/min x0 x1) (Math/max x0 x1)]
        [ya yb] [(Math/min y0 y1) (Math/max y0 y1)]]
    (if (and (<= xa x xb) (<= ya y yb))
      true
      false
      )))

(defn contains-rectangle? [outer inner]
  (let [[[x0 y0] [x1 y1]] inner]
    (if (and (contains-point? outer (point x0 y0))
             (contains-point? outer (point x1 y0))
             (contains-point? outer (point x0 y1))
             (contains-point? outer (point x1 y1)))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [coll] (get coll 1))]
    (map get-second collection)))

; although the course wants a helper function,
; it seems that this is an easier way to do it
(defn second-elements-alternate [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle 
  "removes elem from a-set if it's there, otherwise adds it"
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author?
  "first, convert to new-book representation, then query authors"
  [book author]
  (let [new-book (old-book->new-book book)]
    (contains? (:authors new-book) author)))

(defn authors
  "return all authors as a set"
  [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond 
      (nil? birth-year) author-name
      (nil? death-year) (str author-name " (" birth-year " - )")
      :else (str author-name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)
        how-many (cond
                   (= n 0) "No books."
                   (= n 1) "1 book. "
                   :else (str n " books. "))]
    (if (= 0 n)
      how-many
      (str how-many
           (apply str (interpose ". " (map book->string books)))
           "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter
           (fn [author] (= name (:name author)))
           authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book))
          books))

; %________%
