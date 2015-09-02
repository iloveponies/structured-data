(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[fst snd trd]]
  (+ fst trd))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[lx] [rx]]]
  (- rx lx))

(defn height [[[_ by] [_ ty]]]
  (- ty by))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [point-x (first point)
        point-y (second point)
        rec-x1 (-> rectangle first first)
        rec-x2 (-> rectangle second first)
        rec-y1 (-> rectangle first second)
        rec-y2 (-> rectangle second second)]
    (and (<= rec-x1 point-x rec-x2)
         (<= rec-y1 point-y rec-y2))))

(defn contains-rectangle? [outer inner]
  (= (contains-point? outer (first inner))
     (contains-point? outer (second inner))))

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
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (let [toggle-fn (if (contains? a-set elem)
    disj
    conj)]
    (toggle-fn a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [old-authors (:authors book)
        new-authors (set old-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (boolean (some #(= author %) (:authors book))))

(defn authors [books]
  (set (mapcat :authors books)))

(defn all-author-names [books]
  (->> (authors books)
       (map :name)
       (set)))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
        nil->empty-string (fn [value] (if (nil? value) "" value))]
    (if (or birth death)
      (str name " (" birth " - " (nil->empty-string death) ")")
      name)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)
        authors-string (authors->string authors)]
  (str title ", written by " authors-string)))

(defn books->string [books]
  (let [book-count (count books)
        books-str (apply str (interpose ". " (map book->string books)))]
    (condp = book-count
      0 "No books."
      1 (str "1 book. " books-str ".")
      (str book-count " books. " books-str "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
