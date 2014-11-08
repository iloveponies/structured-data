(ns structured-data)

(defn do-a-thing [x]
  (let [x-plus-x (+ x x)]
    (Math/pow x-plus-x x-plus-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v0 v1 v2] v]
    (+ v0 v2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[ax ay] [bx by]] rectangle]
    (- bx ax)))

(defn height [rectangle]
  (let [[[ax ay] [bx by]] rectangle]
    (- by ay)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[ax ay] [bx by]] rectangle
        [px py] point]
    (and (<= ax px bx) (<= ay py by))))

(defn contains-rectangle? [outer inner]
  (let [[inner-a inner-b] inner]
    (and (contains-point? outer inner-a)
         (contains-point? outer inner-b))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [c] (get c 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
               (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        years (str " (" birth-year " - " death-year ")")]
    (if birth-year
      (str name years)
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [lkm (count books)
        lkm-string (cond
                    (== lkm 0) "No books."
                    (== lkm 1) "1 book. "
                    :else (str lkm " books. "))
        books-string (apply str (interpose ". " (map book->string books)))]
    (if (== lkm 0)
      lkm-string
      (str lkm-string books-string "."))))

(defn books-by-author [author books]
  (let [has-this-author? (fn [b] (has-author? b author))]
    (filter has-this-author? books)))

(defn author-by-name [name authors]
  (let [filtered-authors (filter (fn [a] (= (:name a) name)) authors)]
    (if (== (count filtered-authors) 0)
      nil
      (first filtered-authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
