(ns structured-data)

(defn do-a-thing [x]
  (let [doubled (+ x x)]
    (Math/pow doubled doubled)))

(defn spiff [v]
  (let [n1 (or (get v 0) 0)
        n3 (or (get v 2) 0)]
    (+ n1 n3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[n1 _ n3] v]
    (+ n1 n3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]]
  (Math/abs (- x2 x1)))

(defn height [[[_ y1] [_ y2]]]
  (Math/abs (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[left-x bottom-y] [right-x top-y]]                        [x y]]
  (and (<= left-x x right-x) (<= bottom-y y top-y)))

(defn contains-rectangle? [outer [bottom-left top-right]]
  (and 
   (contains-point? outer bottom-left)
   (contains-point? outer top-right)))

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
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [selector (fn [v] (get v 1))]
    (map selector collection)))

(defn titles [books]
  (map :title books))

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
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (:authors book)) books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth (:birth-year author)]
    (if birth
      (let [death (or (:death-year author) "")
            years-string (str "(" birth " - " death ")")]
        (str author-name  " " years-string))
      author-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)
        author-string (authors->string authors)]
    (str title ", written by " author-string)))


(defn books->string [books]
  (let [num-books (count books)]
    (if (= num-books 0)
      "No books."
      (let [summary
            (if (= num-books 1)
              "1 book."
              (str num-books " books."))
            books-listing (str (apply str (interpose ". " (map book->string books))) ".")]
        (str summary " " books-listing)))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
