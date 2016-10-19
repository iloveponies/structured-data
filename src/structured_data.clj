(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)
        pow (Math/pow xx xx)]
    pow))


(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (if (empty? v)
      "?"
      (+ first third))))


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[fst scnd trd] v
        sum (+ fst trd)]
    (if (empty? v)
      "?"
      sum)))


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
  (let [w (width rectangle)
        h (height rectangle)]
    (= w h)))


(defn area [rectangle]
  (* (width rectangle)(height rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and(<= x1 x3 x2)(<= y1 y3 y2))))


(defn contains-rectangle? [outer inner]
  (let [fst (first inner)
        scnd (second inner)]
    (and(contains-point? outer fst)
            (contains-point? outer scnd))))


(defn title-length [book]
  (count (get book :title)))


(defn author-count [book]
  (count (get book :authors)))


(defn multiple-authors? [book]
  (let [authors (author-count book)]
    (> authors 1)))


(defn add-author [book new-author]
  (let [all-authors (conj (get book :authors) new-author)
        new-book (assoc book :authors all-authors)]
    new-book))


(defn alive? [author]
  (not(contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [second-of-sequence (fn [x] (get x 1 nil))]
    (map second-of-sequence collection)))


(defn titles [books]
  (map :title books))


(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


(defn monotonic? [a-seq]
  (or(apply <= a-seq)(apply >= a-seq)))


(defn stars [n]
  (apply str(repeat n "*")))


(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (> (count a-seq) (count a-set))))


(defn old-book->new-book [book]
  (let [authors-set (set(:authors book))
        new-book (assoc book :authors authors-set)]
    new-book))


(defn has-author? [book author]
  (let [books-authors (:authors book)]
    (contains? books-authors author)))


(defn authors [books]
  (let [all-authors (fn [book] (:authors book))]
    (set(apply concat (map all-authors books)))))


(defn all-author-names [books]
  (set(map :name (authors books))))


(defn author->string [author]
  (let [year (fn [x]
    (cond
      (:death-year x)(str " (" (:birth-year x) " - " (:death-year x) ")")
      (:birth-year x)(str " (" (:birth-year x) " - )")
      :else (str "")))]
    (str (:name author)(year author))))


(defn authors->string [authors]
  (let [author-strings (fn [author] (author->string author))]
    (apply str(interpose ", " (map author-strings authors)))))


(defn book->string [book]
  (let [book-name (:title book)
        authors-strings (authors->string (:authors book))
        book-string (str book-name ", written by " authors-strings)]
    book-string))


(defn books->string [books]
  (let [book-count (count books)]
    (cond
      (== book-count 0) "No books."
      (== book-count 1) (str book-count " book. " (book->string (first (seq books)))".")
      :else (str book-count " books. " (apply str (interpose ". " (map book->string books))) "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (first(filter (fn [author] (= (:name author) name)) authors)))


(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

