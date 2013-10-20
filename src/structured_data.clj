(ns structured-data)

(defn do-a-thing
  [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff
  [v]
  (+ (first v) (first (drop 2 v))))

(defn cutify
  [v]
  (conj v "<3"))

(defn spiff-destructuring
  [[x _ y]]
  (+ x y))

(defn point
  [x y]
  [x y])

(defn rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn width
  [[[x1 _] [x2 _]]]
  (Math/abs (- x1 x2)))

(defn height 
  [[[_ y1] [_ y2]]]
  (Math/abs (- y1 y2)))

(defn square?
  [rectangle]
  (= (height rectangle)
     (width rectangle)))

(defn area
  [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn contains-point?
  [[[x1 y1] [x2 y2]] [x3 y3]]
  (and (<= x1 x3 x2)
       (<= y1 y3 y2)))

(defn contains-rectangle?
  [outer [bottom-left top-right]]
  (and (contains-point? outer bottom-left)
       (contains-point? outer top-right)))

(defn title-length
  [{title :title}]
  (count title)) 

(defn author-count
  [{authors :authors}]
  (count authors))

(defn multiple-authors?
  [book]
  (< 1 (author-count book)))

(defn add-author
  [book new-author]
  (update-in book [:authors] conj new-author))

(defn alive?
  [{death-year :death-year}]
  (nil? death-year))

(defn element-lengths
  [collection]
  (map count collection))

(defn second-elements
  [collection]
  (map second collection))

(defn titles
  [books]
  (map :title books))

(defn monotonic?
  [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars
  [n]
  (apply str (repeat n "*")))

(defn toggle
  [a-set elem]
  (if (a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates?
  [a-seq]
  (not (apply distinct? a-seq)))

(defn old-book->new-book
  [book]
  (update-in book [:authors] set))

(defn has-author?
  [book author]
  (not (nil? (get-in book [:authors author]))))

(defn authors
  [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names
  [books]
  (set (map :name (authors books))))

(defn author->string
  [{:keys [name birth-year death-year]}]
  (if birth-year
    (str name " (" birth-year " - " death-year ")")
    name))

(defn authors->string
  [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string
  [{:keys [title authors]}]
  (str title ", written by " (authors->string authors)))

(defn books->string
  [books]
  (let [book-count (count books)
        books->string #(clojure.string/join ", " (map book->string %))]
    (cond (zero? book-count) "No books."
          (= 1 book-count) (str "1 book. " (books->string books) ".")
          :else (str book-count " books. " (books->string books) "."))))

(defn books-by-author
  [author books]
  (filter #(contains? (:authors %) author) books))

(defn author-by-name
  [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors
  [authors]
  (remove :death-year authors))

(defn has-a-living-author?
  [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors
  [books]
  (filter has-a-living-author? books))

; %________%
