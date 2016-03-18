(ns structured-data)

(defn do-a-thing [x]
  (let [c (+ x x)]
  (Math/pow c c)))

(defn spiff [v]
  (let [a (get v 0)
        b (if (get v 2) (get v 2) 0)]
    (+ a b)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (if c (+ a c) a)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (Math/abs (int (- (first (second rectangle)) (first (first rectangle))))))

(defn height [rectangle]
  (Math/abs (int (- (second (second rectangle)) (second (first rectangle))))))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (if (and (<= (first (first rectangle)) (first point) (first (second rectangle)))
           (<= (second (first rectangle)) (second point) (second (second rectangle))))
    true false))

(defn contains-rectangle? [outer inner]
  (if (every? true? (map #(contains-point? outer %) (map vector (first inner) (second inner))))
    true false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (get (:authors book) 1) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new (conj authors new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (if (not (:death-year author)) true false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(second %) collection))

(defn titles [books]
  (map #(:title %) books))

(defn monotonic? [a-seq]
  (let [u (apply <= a-seq)
        d (apply >= a-seq)]
    (or u d)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (= (count a-set) (count a-seq)) false true)))

(defn old-book->new-book [book]
  (let [old (:authors book)
        new (set old)]
    (assoc book :authors new)))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
  (set (apply concat (map #(:authors %) books))))

(defn all-author-names [books]
  (let [authors (authors books)]
    (set (map #(:name %) authors))))

(defn author->string [author]
  (let [name (:name author)
        year-string (if (:birth-year author)
                      (str " (" (:birth-year author) " - " (:death-year author) ")") "")]
    (str name year-string)))

(defn authors->string [authors]
  (let [interpose (interpose ", " (map author->string authors))]
    (apply str interpose)))

(defn book->string [book]
  (let [name (:title book)
        authors (authors->string (:authors book))]
    (str name ", written by " authors)))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [c (count books)
          first (if (= c 1) " book. " " books. ")
          books (apply str (interpose ". " (map book->string books)))]
      (str c first books "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
