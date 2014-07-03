(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (if (or (nil? first)
            (nil? third))
      nil
      (+ first third))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x y] [x2 y2]] rectangle]
    (- x2 x)))

(defn height [rectangle]
  (let [[[x y] [x2 y2]] rectangle]
    (- y2 y)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[point-x point-y] point
        [[x y] [x2 y2]] rectangle]
    (and (<= x point-x x2)
         (<= y point-y y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-top inner-bottom] inner]
    (and (contains-point? outer inner-top)
         (contains-point? outer inner-bottom))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [elem] (first (rest elem)))]
    (map get-second collection)))

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
  (let [no-dup (set a-seq)]
    (not (= (count no-dup) (count a-seq)))))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [get-authors (fn [book] (:authors book))]
    (apply clojure.set/union (map get-authors books))))

(defn all-author-names [books]
  (let [get-names (fn [author] (:name author))]
    (set (map get-names (authors books)))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        dates (str (when-not (nil? birth-year)
                     (str " ("
                          birth-year
                          " - "
                          (when-not (nil? death-year)
                            death-year)
                          ")")))]
    (str author-name dates)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (str ", written by " (authors->string (:authors book)))]
    (str title authors)))

(defn books->string [books]
  (let [book-count (count books)]
    (str (case book-count
           0 "No books."
           1 "1 book. "
           (str book-count " books. "))
         (apply str (interpose ". " (map book->string books)))
         (when-not (= 0 book-count) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
