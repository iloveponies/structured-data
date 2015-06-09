(ns structured-data)

(defn do-a-thing [x]
  (let [s (+ x x)]
    (Math/pow s s)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))  

(defn spiff-destructuring [[x1 _ x2]]
  (+ x1 x2))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [x3 y3]          point]
    (and
      (<= x1 x3 x2)
      (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and
      (contains-point? outer p1)
      (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [curr-authors (:authors book)
        new-authors  (conj curr-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [snd-fn (fn [x] (second x))]
    (map snd-fn collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-seq-as-set (set a-seq)]
    (not= (count a-seq) (count a-seq-as-set))))

(defn old-book->new-book [book]
  (let [authors-as-set (set (:authors book))]
    (assoc book :authors authors-as-set)))

(defn has-author? [book author]
  (not (nil? (get (:authors book) author))))

(defn authors [books]
  (apply 
    clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [death-year (:death-year author)
        birth-year (:birth-year author)
        years (cond 
                (not (nil? death-year)) (str "(" birth-year " - " death-year ")")
                (not (nil? birth-year)) (str "(" birth-year " - " ")")
                :else                   "")]
    (clojure.string/trim
      (str (:name author) " " years))))


(defn authors->string [authors]
  (let [authors-as-string (map author->string authors)]
    (apply str (interpose ", " authors-as-string))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [count-string (cond 
                       (= 0 (count books)) "No books."
                       (= 1 (count books)) "1 book."
                       :else               (str (count books) " books."))
        book-string (apply str (map #(str " " (book->string %) ".") books))]
      (str count-string book-string)))

(defn books-by-author [author books]
  (filter #(contains? (:authors %) author) books))

(defn author-by-name [name authors]
  (first
    (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
