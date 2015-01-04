(ns structured-data)

(defn do-a-thing [x]
  (let [two-x (+ x x)]
    (Math/pow two-x two-x)))

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

(defn width [[[xbl ybl] [xtr ytr]]]
  (- xtr xbl))

(defn height [[[xbl ybl] [xtr ytr]]]
  (- ytr ybl))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[xbl ybl] [xtr ytr]] rectangle
        [x y] point]
    (and (<= xbl x xtr)
         (<= ybl y ytr))))


(defn contains-rectangle? [outer inner]
  (let [[ibl itr] inner]
    (and (contains-point? outer ibl)
         (contains-point? outer itr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)]
    (assoc book :authors (conj old-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)))

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
  (let [old-authors (:authors book)]
    (assoc book :authors (set old-authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nm (:name author)
        by (:birth-year author)
        dy (:death-year author)]
    (cond 
      dy (str nm " (" by " - " dy ")")
      by (str nm " (" by " - )")
      :else nm)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [nbooks (count books)
        book-strings (map book->string books)
        books-desc (apply str (concat (interpose ". " book-strings) "."))]
    (cond 
      (= 0 nbooks) "No books."
      (= 1 nbooks) (str "1 book. " books-desc)
      :else (str nbooks " books. " books-desc))))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
