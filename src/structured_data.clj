(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y )))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0 _] [x1 _]] rectangle]
    (Math/abs (- x0 x1))))

(defn height [rectangle]
  (let [[[_ y0] [_ y1]] rectangle]
    (Math/abs (- y0 y1))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[p-x p-y] point
        [[r-x0 r-y0] [r-x1 r-y1]] rectangle]
    (and (<= r-x0 p-x r-x1)
         (<= r-y0 p-y r-y1))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left upper-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer upper-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [xs] (get xs 1))]
    (map snd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq)
        (count (set a-seq))))

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
  (let [{name :name
         birth-year :birth-year
         death-year :death-year} author]
    (str name
         (if-not birth-year
           ""
           (str " (" birth-year " - " (or death-year "") ")")))))

(defn authors->string [authors]
  (let [strs (map author->string authors)]
    (apply str (interpose ", " strs))))

(defn book->string [book]
  (let [{title :title
         authors :authors} book]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (str
     (let [book-count (count books)
           berks (str "book" (if (= 1 book-count) "" "s"))]
       (str book-count " " berks ". "))
     (apply str (interpose ". " (map book->string books)))
     ".")))

(defn books-by-author [author books]
  (filter
   (fn [book] (has-author? book author))
   books))

(defn author-by-name [name authors]
  (let [has-name? (fn [author]
                    (= name (:name author)))]
    (first
     (filter has-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
