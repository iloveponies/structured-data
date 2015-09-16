(ns structured-data)

(defn do-a-thing [x]
  (let [thing (+ x x)]
    (Math/pow thing thing)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (if (nil? b)
      a
      (+ a b))
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[a1 b1] [a2 b2]] inner]
    (and (<= x1 a1 a2 x2) (<= y1 b1 b2 y2)))
  )

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
  (map #(count %) collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (mapcat :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [{:keys [name birth-year death-year]} author
        birth-info (if (nil? birth-year)
                     ""
                     (str " (" birth-year " - " death-year ")"))]
    (str name birth-info)))

(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)) ))

(defn books->string [books]
  (let [c (count books)]
    (cond 
      (= c 0) "No books."
      (= c 1) (str "1 book. " (book->string (get books 0)) ".")
      :else (str c " books. " (clojure.string/join ", " (map book->string books)) "."))
    ))

(defn books-by-author [author books]
  (filter #(contains? (:authors %) author) books))

(defn author-by-name [name authors]
  (let [res (filter #(= (:name %) name) authors)]
    (if (seq res)
      (nth res 0)
      nil))
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (seq (living-authors (:authors book)))
    true
    false))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
