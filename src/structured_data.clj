(ns structured-data)

(defn do-a-thing [x]
  (let [xplusx (+ x x)]
        (Math/pow xplusx xplusx)))

(defn spiff [v]
  (+ (or (get v 0) 0) (or (get v 2) 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ (or a 0) (or c 0))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1][x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1][x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and (<= x1 x x2)
       (<= y1 y y2)))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1 ))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
    (map second-element collection)))

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
  (not= (seq (set a-seq)) a-seq))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
   (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let[auths (authors books)]
    (set (map :name auths))))

(defn author->string [author]
  (let[by (:birth-year author)
       dy (:death-year author)
       name (:name author)]
    (if by
      (str name " (" by " - " dy ")")
      name)))

(defn authors->string [authors]
  (apply str
         (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (let [bc (count books)
        c (cond
           (= 0 bc) "No books."
           (= 1 bc) "1 book."
           :else (str bc " books."))
        bookstring (fn [b] (str (book->string b) "."))]
    (apply str (interpose " "(conj (map bookstring books) c)))))

(defn has-author*? [author book]
  (contains? (:authors book) author))

(defn books-by-author [author books]
  (filter (fn[b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn[a] (= name (:name a))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
